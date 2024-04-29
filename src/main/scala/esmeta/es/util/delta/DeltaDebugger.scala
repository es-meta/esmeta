package esmeta.es.util.delta

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.mutator.Util
import esmeta.util.BasicWalker
import esmeta.util.BaseUtils.error

// A hierarchical delta debugger
class DeltaDebugger(
  cfg: CFG,
  checker: (code: String) => Boolean, // true if bug exists
  detail: Boolean = false,
) {
  lazy val grammar = cfg.grammar

  lazy val scriptParser: AstFrom = cfg.scriptParser

  def result(code: String) = {
    val ast = scriptParser.from(code)
    DeltaWalker.result(ast).toString(grammar = Some(grammar))
  }

  private def log(x: Any): Unit = if (detail) println(s"[dd] $x")

  object DeltaWalker extends AstWalker {
    // skip visited Syn node
    var visited: Set[Syntactic] = Set.empty

    // tracking minimal Ast to retrieve result
    var minimal: List[Ast] = List.empty

    def result(ast: Ast): Ast =
      minimal = List(ast)
      walk(ast, ast => ast)
      minimal.head

    def walk(syn: Syntactic, f: Ast => Ast): Syntactic =
      val Syntactic(name, args, rhsIdx, children) = syn
      name match
        case "StatementList" if !visited.contains(syn) =>
          log(s"walk: statementList")
          val statementListItems = flattenStatementList(syn)
          if (statementListItems.size < 2) baseWalk(syn, f)
          else
            val d = MinDD(
              statementListItems.size,
              mask => {
                log(s"walk/mask: $mask")
                val items = (statementListItems zip mask).filter(_._2).map(_._1)
                foldStatementList(items) match
                  case Some(statementList) =>
                    val code =
                      f(statementList).toString(grammar = Some(grammar))
                    checker(code)
                  case None => false
              },
              detail,
            ).result
            val items = (statementListItems zip d).filter(_._2).map(_._1)
            foldStatementList(items) match
              case Some(statementList) =>
                minimal +:= f(statementList)
                baseWalk(statementList, f)
              case None => error("Failed to get StatementList")
        case _ => baseWalk(syn, f)

    def baseWalk(syn: Syntactic, f: Ast => Ast): Syntactic =
      syn match
        case Syntactic(name, args, rhsIdx, children) =>
          var base = children
          for {
            (child, index) <- children.zipWithIndex
          } {
            walkOpt(
              child,
              walk(
                _,
                ast =>
                  f(
                    Syntactic(
                      name,
                      args,
                      rhsIdx,
                      base.updated(index, Some(ast)),
                    ),
                  ),
              ),
            ) match
              case Some(syn) => base.updated(index, Some(syn))
              case None      =>
          }
          Syntactic(name, args, rhsIdx, base)

    private def flattenStatementList(ast: Syntactic): List[Syntactic] =
      ast match
        // singleton (-> StatementListItem)
        case Syntactic("StatementList", args, 0, children) =>
          children.collect {
            case Some(child @ Syntactic(_, _, _, _)) => child
          }.toList
        // list (-> StatementList :: StatementListItem)
        case Syntactic("StatementList", args, 1, children) =>
          children.toList match
            case Some(
                  list @ Syntactic(
                    "StatementList",
                    _,
                    _,
                    _,
                  ),
                ) :: Some(
                  listItem @ Syntactic(
                    "StatementListItem",
                    _,
                    _,
                    _,
                  ),
                ) :: Nil =>
              flattenStatementList(list) :+ listItem
            case _ => error("Unexpected Ast @ flattenStatementList")
        case _ => error("Unexpected Ast @ flattenStatementList")

    private def foldStatementList(
      items: List[Syntactic],
      head: Option[Syntactic] = None,
    ): Option[Syntactic] = items match
      case Nil => head
      case h :: t =>
        h match
          case item @ Syntactic("StatementListItem", args, _, _) =>
            val newHead = head match
              case Some(_) =>
                Syntactic("StatementList", args, 1, Vector(head, Some(item)))
              case None =>
                Syntactic("StatementList", args, 0, Vector(Some(item)))
            visited += newHead; foldStatementList(t, Some(newHead))
          case _ => error("Unexpected Ast @ foldStatementList")
  }
}
trait AstWalker extends BasicWalker {
  // Ast
  def walk(ast: Ast, f: Ast => Ast): Ast = ast match
    case syn: Syntactic => walk(syn, f)
    case lex: Lexical   => walk(lex, f)

  // Syntactic production
  def walk(syn: Syntactic, f: Ast => Ast): Syntactic

  // Lexical production
  def walk(lex: Lexical, f: Ast => Ast): Lexical = lex
}
