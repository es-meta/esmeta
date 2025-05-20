package esmeta.transpile.util

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.UnitWalker
import esmeta.spec.{Spec, Nonterminal, Terminal}
import esmeta.util.Appender

class AstPrinter(app: Appender, cfg: CFG) {
  given CFG = cfg

  private def merge(
    a: List[Terminal | Unit],
    b: List[Ast],
  ): List[Terminal | Ast] = {
    (a, b) match
      case (Nil, Nil) => Nil
      case (Nil, rest) =>
        throw new IllegalArgumentException(
          s"Arity Failure: Cannot merge production $a with ast value $b, too many children given.",
        )
      case ((term: Terminal) :: tail, _) => term :: merge(tail, b)
      case (() :: tail, Nil) =>
        throw new IllegalArgumentException(
          s"Arity Failure: Cannot merge production $a with ast value $b, too few children given.",
        )
      case (() :: tail, node :: rest) => node :: merge(tail, rest)
  }

  def append(ast: Ast): Unit = ast match
    case Lexical(name, str) => app >> str >> " "
    // please note that idx is rhsIdx by definition, subIdx is not related to rhsIdx
    case syn @ Syntactic(name, args, rhsIdx, children) =>
      val foundGrammar = cfg.grammar.prods
        .find(_.name == name)
      // None == Reduced Optional
      val foundRhs = foundGrammar
        .flatMap(_.rhsVec.lift(syn.idx))
        .map(_.getSymbols(syn.subIdx))
        .getOrElse(???)
      val tnts = foundRhs
        .flatMap(identity) // remove ommitted optionals
        .flatMap[Nonterminal | Terminal] { sym =>
          sym.getT.orElse(sym.getNt)
        } // remove not-nonterminals, such as [empty].
        .map {
          case nt: Nonterminal => ()
          case t: Terminal     => t
        } // replace nonterminals with () to simplify merge function
      // assert : tnts.length == children.toList.filter(_.isDefined).length
      assert(
        tnts.filter(_ == ()).length == children.toList
          .filter(_.isDefined)
          .length,
      )
      merge(
        tnts,
        children.toList.flatMap(identity),
      ).foreach {
        _ match
          case Terminal(term) => app >> term >> " "
          case ast: Ast       => append(ast)
      }
}

extension (ast: Ast) {

  /** pretty print the AST */
  def toCode(using CFG): String =
    val appender = Appender()
    val printer = new AstPrinter(appender, summon[CFG])
    printer.append(ast)
    appender.toString
}
