package esmeta.es.util.delta

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.mutator.Util
import esmeta.util.BasicWalker
import esmeta.util.BaseUtils.error
import esmeta.injector.Injector.header
import scala.util.*

// A hierarchical delta debugger
class DeltaDebugger(
  cfg: CFG,
  checker: (code: String) => Boolean, // true if bug exists
  detail: Boolean = false,
) {
  lazy val grammar = cfg.grammar

  lazy val esParser = cfg.esParser
  lazy val scriptParser: AstFrom = cfg.scriptParser

  def result(code: String) = {
    val ast = scriptParser.from(code)
    DeltaWalker.result(ast).toString(grammar = Some(grammar))
  }

  private def log(x: Any): Unit = if (detail) println(s"[dd] $x")

  object DeltaWalker extends AstWalker {
    // tracking minimal Ast to retrieve result
    var minimal: List[Ast] = List.empty

    def result(ast: Ast): Ast =
      minimal = List(ast)
      walk(ast, ast => ast)
      minimal.head

    def walk(syn: Syntactic, f: Ast => Ast): Syntactic =
      val Syntactic(name, args, rhsIdx, children) = syn
      name match
        case "StatementList" | "CaseBlock" =>
          log(s"walk: $name"); delta(syn, f)
        case "ArrayLiteral" | "ElementList" | "Elision" | "ObjectLiteral" |
            "PropertyDefinitionList" | "ObjectAssignmentPattern" |
            "AssignmentPropertyList" | "ArrayAssignmentPattern" |
            "AssignmentElementList" | "FormalParameters" |
            "FormalParameterList" =>
          log(s"walk: $name"); delta(syn, f, 1)
        // MemberExpression[1] -> MemberExpression "[" Expression "]"
        case "MemberExpression" if rhsIdx == 1 =>
          val removed = children(0).get.asInstanceOf[Syntactic]
          checker(f(removed).toString(grammar = Some(grammar))) match
            case true =>
              log(s"walk: $name"); minimal +:= f(removed); walk(removed, f)
            case false => baseWalk(syn, f)
        case "OptionalExpression" =>
          val removed = children(0).get.asInstanceOf[Syntactic]
          checker(f(removed).toString(grammar = Some(grammar))) match
            case true =>
              log(s"walk: $name ${f(removed).toString(grammar = Some(grammar))}"); minimal +:= f(removed);
              walk(removed, f)
            case false => baseWalk(syn, f)
        case _ => baseWalk(syn, f)

    def delta(
      syn: Syntactic,
      f: Ast => Ast,
      granularity: Int = 2,
    ): Syntactic =
      val Syntactic(name, args, rhsIdx, children) = syn
      if (children.size < granularity) baseWalk(syn, f)
      else
        val mask = MinDD(
          children.size,
          mask =>
            log(s"walk/mask: $mask")
            val filtered = (children zip mask).map {
              case (astOpt, true) => astOpt
              case (_, false)     => None
            }
            val newSyn = Syntactic(name, args, rhsIdx, filtered)
            Try {
              esParser(name, args).from(
                newSyn.toString(grammar = Some(grammar)),
              )
            }.isSuccess match {
              case true =>
                checker(
                  f(Syntactic(name, args, rhsIdx, filtered))
                    .toString(grammar = Some(grammar)),
                )
              case _ => false
            }
          ,
          detail,
        ).result(granularity)
        val filtered = (children zip mask).map {
          case (astOpt, true) => astOpt
          case (_, false)     => None
        }
        val newSyn = Syntactic(name, args, rhsIdx, filtered)
        val validSyn = esParser(name, args)
          .from(
            newSyn.toString(grammar = Some(grammar)),
          )
          .asInstanceOf[Syntactic]
        minimal +:= f(validSyn)
        baseWalk(validSyn, f)

    // just walk and forward AST constructor function `f`
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
              case Some(syn) => base = base.updated(index, Some(syn))
              case None      =>
          }
          Syntactic(name, args, rhsIdx, base)

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
