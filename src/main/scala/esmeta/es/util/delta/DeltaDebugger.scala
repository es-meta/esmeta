package esmeta.es.util.delta

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.mutator.Util
import esmeta.util.BasicWalker
import esmeta.util.BaseUtils.error
import esmeta.injector.Injector.header

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
          log(s"walk: StatementList")
          delta(syn, f, flattenStatementList, foldStatementList)
        case "CaseBlock" if !visited.contains(syn) =>
          log(s"walk: CaseBlock")
          delta(syn, f, flattenCaseBlock, foldCaseBlock(syn))
        case _ => baseWalk(syn, f)

    def delta(
      syn: Syntactic,
      f: Ast => Ast,
      flat: Syntactic => List[Syntactic],
      fold: (List[Syntactic], Option[Syntactic]) => Option[Syntactic],
    ): Syntactic =
      val items = flat(syn)
      if (items.size < 2) baseWalk(syn, f)
      else
        val mask = MinDD(
          items.size,
          mask =>
            log(s"walk/mask: $mask")
            val filtered = (items zip mask).filter(_._2).map(_._1)
            fold(filtered, None).fold(false)(list =>
              checker(f(list).toString(grammar = Some(grammar))),
            )
          ,
          detail,
        ).result
        val filtered = (items zip mask).filter(_._2).map(_._1)
        fold(filtered, None) match
          case Some(list) => minimal +:= f(list); baseWalk(list, f)
          case None       => error("Failed to get list")

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

    private def flattenStatementList(syn: Syntactic): List[Syntactic] =
      syn.getItems("StatementListItem").asInstanceOf[List[Syntactic]]

    private def foldStatementList(
      items: List[Syntactic],
      head: Option[Syntactic] = None,
    ): Option[Syntactic] =
      foldList(items, head, "StatementList", "StatementListItem")

    private def foldList(
      items: List[Syntactic],
      head: Option[Syntactic],
      listName: String,
      itemName: String,
    ): Option[Syntactic] = items match
      case Nil => head
      case h :: t =>
        h match
          case item @ Syntactic(itemName, args, _, _) =>
            val newHead = head match
              case Some(_) =>
                Syntactic(listName, args, 1, Vector(head, Some(item)))
              case None =>
                Syntactic(listName, args, 0, Vector(Some(item)))
            visited += newHead; foldList(t, Some(newHead), listName, itemName)

    private def flattenCaseBlock(syn: Syntactic): List[Syntactic] =
      syn match
        // CaseBlock[0] -> "{" CaseClauses? "}"
        case Syntactic("CaseBlock", args, 0, children) =>
          children.toList match
            case Some(clauses) :: Nil =>
              clauses.getItems("CaseClause").asInstanceOf[List[Syntactic]]
            case _ => Nil
        // CaseBlock[1] -> "{" CaseClauses? DefaultClause CaseClauses? "}"
        case Syntactic("CaseBlock", args, 1, children) =>
          children.toList match
            case cs1 :: dc :: cs2 :: Nil =>
              (cs1.fold(Nil)(_.getItems("CaseClause")) ++
              (dc.get :: cs2.fold(Nil)(_.getItems("CaseClause"))))
                .asInstanceOf[List[Syntactic]]
            case _ => error("Expected CaseBlock[1] @ flattenCaseBlock")
        case _ => error("Unexpected Ast @ flattenCaseBlock")

    private def foldCaseBlock(
      syn: Syntactic,
    )(items: List[Syntactic], head: Option[Syntactic]): Option[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = syn
      items.find(p => p.name == "DefaultClause") match
        case Some(default) =>
          items.splitAt(items.indexOf(default)) match
            case (css1, d :: css2) =>
              val cs1 = foldList(css1, None, "CaseClauses", "CaseClause")
              val cs2 = foldList(css2, None, "CaseClauses", "CaseClause")
              Some(
                Syntactic(
                  name,
                  args,
                  rhsIdx,
                  Vector(cs1, Some(d), cs2),
                ),
              )
            case _ => error("Unexpected CaseBlock @ foldCaseBlock")
        case None =>
          Some(
            Syntactic(
              name,
              args,
              0,
              Vector(foldList(items, None, "CaseClauses", "CaseClause")),
            ),
          )
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
