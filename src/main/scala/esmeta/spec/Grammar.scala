package esmeta.spec

import Stringifier.*

/** grammars */
case class Grammar(
  prods: List[Production],
  prodsForWeb: List[Production],
) extends SpecElem

/** productions */
case class Production(
  lhs: Lhs,
  kind: Production.Kind,
  oneof: Boolean,
  rhsList: List[Rhs],
) extends SpecElem
object Production extends Parser[Production]:
  enum Kind extends SpecElem:
    case Syntactic, Lexical, NumericString

/** production left-hand-sides (LHSs) */
case class Lhs(name: String, params: List[String]) extends SpecElem
object Lhs extends Parser[Lhs]

/** production alternative right-hand-sides (RHSs) */
case class Rhs(
  condition: Option[RhsCond],
  symbols: List[Symbol],
  id: Option[String],
) extends SpecElem
object Rhs extends Parser[Rhs]

/** condidtions for RHSs */
case class RhsCond(name: String, pass: Boolean) extends SpecElem
object RhsCond extends Parser[RhsCond]

/** grammar symbols */
enum Symbol extends SpecElem:
  /** terminal symbols */
  case Terminal(term: String)

  /** nonterminal symbols */
  case Nonterminal(name: String, args: List[NtArg], optional: Boolean)

  /** butnot symbols */
  case ButNot(base: Nonterminal, cases: List[Symbol])

  /** but-only-if symbols */
  case ButOnlyIf(base: Nonterminal, methodName: String, cond: String)

  /** lookahead symbols */
  case Lookahead(contains: Boolean, cases: List[List[Symbol]])

  /** empty symbols */
  case Empty

  /** no-line-terminator symbols */
  case NoLineTerminator

  /** symbols for code point abbreviations */
  case CodePointAbbr(abbr: String)

  /** symbols for sets of unicode code points with a condition */
  case UnicodeSet(cond: Option[String])
object Symbol extends Parser[Symbol]

/** nonterminal arguments */
case class NtArg(kind: NtArg.Kind, name: String) extends SpecElem
object NtArg extends Parser[NtArg]:
  enum Kind extends SpecElem:
    case True, False, Pass
