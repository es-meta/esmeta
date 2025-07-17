package esmeta.verify

import esmeta.cfg.*
import esmeta.es.*
import esmeta.spec.*

def resolveSymbol(symbol: Symbol): Option[Symbol] = {
  symbol match
    case t: Terminal                       => Some(t)
    case nt: Nonterminal                   => Some(nt)
    case Optional(symbol)                  => resolveSymbol(symbol)
    case ButNot(base, notCases)            => resolveSymbol(base)
    case ButOnlyIf(base, methodName, cond) => resolveSymbol(base)
    case Lookahead(contains, cases)        => None
    case Empty                             => None
    case NoLineTerminator                  => None
    case CodePoint(cp, desc)               => println("not sure"); None
    case CodePointAbbr(abbr)               => println("not sure"); None
    case UnicodeSet(cpCond)                => println("not sure"); None

}

extension (prod: Production)
  def possibleChains(using CFG): List[List[Production]] = {
    prod match
      case Production(lhs, kind, oneof, rhsVec) =>
        for {
          (rhs, rhsIdx) <- rhsVec.zipWithIndex
        } yield {
          val Lhs(name, params) = lhs
          val Rhs(conditions, symbols, id) = rhs
          symbols.map(_.getNt)
          symbols.map {
            _ match
              case Terminal(term)                    =>
              case Nonterminal(name, args)           =>
              case Optional(symbol)                  =>
              case ButNot(base, notCases)            =>
              case ButOnlyIf(base, methodName, cond) =>
              case Lookahead(contains, cases)        =>
              case Empty                             =>
              case NoLineTerminator                  =>
              case CodePoint(cp, desc)               =>
              case CodePointAbbr(abbr)               =>
              case UnicodeSet(cpCond)                =>
          }
          ???
        }
  }
end extension

extension (ast: Ast)
  def possibleChains(using CFG): List[List[Ast]] = {
    ast match
      case lex: Lexical => List(Nil)
      case syn: Syntactic =>
        syn.children.flatten match {
          case Vector(child) => child.possibleChains.map(ast :: _)
          case _             => List(List(ast))
        }
      case hole @ Hole(name, args, label, attrs) =>
        for {
          prod <- summon[CFG].grammar.prods
          Production(lhs, kind, oneof, rhsVec) = prod
        } yield {
          ???
        }
        ???
  }

  /** get syntax-directed operation (SDO) */
  def getSdo(name: String)(using cfg: CFG): List[(Ast, Func)] = {
    val fnameMap = cfg.fnameMap
    val sdos = for {
      chains <- ast.possibleChains
    } yield chains.foldLeft[Option[(Ast, Func)]](None) {
      case (None, ast0) =>
        val subIdx = ast0.subIdx
        val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$name"
        fnameMap
          .get(fname)
          .orElse(fnameMap.get(s"<DEFAULT>.$name"))
          .map((ast0, _))
      case (res: Some[_], _) => res
    }
    sdos.flatten
  }

end extension
