package esmeta.spec

import esmeta.spec.util.*

// -----------------------------------------------------------------------------
// grammars
// -----------------------------------------------------------------------------
case class Grammar(
  prods: List[Production],
  prodsForWeb: List[Production],
) extends SpecElem {
  lazy val nameMap: Map[String, Production] =
    (for (prod <- prods) yield prod.lhs.name -> prod).toMap
}

// -----------------------------------------------------------------------------
// productions
// -----------------------------------------------------------------------------
case class Production(
  lhs: Lhs,
  kind: Production.Kind,
  oneof: Boolean,
  rhsList: List[Rhs],
) extends SpecElem
object Production extends Parser.From[Production]:
  enum Kind extends SpecElem:
    case Syntactic, Lexical, NumericString

// -----------------------------------------------------------------------------
// production left-hand-sides (LHSs)
// -----------------------------------------------------------------------------
case class Lhs(name: String, params: List[String]) extends SpecElem
object Lhs extends Parser.From[Lhs]

// -----------------------------------------------------------------------------
// production alternative right-hand-sides (RHSs)
// -----------------------------------------------------------------------------
case class Rhs(
  condition: Option[RhsCond],
  symbols: List[Symbol],
  id: Option[String],
) extends SpecElem
object Rhs extends Parser.From[Rhs]

// -----------------------------------------------------------------------------
// condidtions for RHSs
// -----------------------------------------------------------------------------
case class RhsCond(name: String, pass: Boolean) extends SpecElem
object RhsCond extends Parser.From[RhsCond]

// -----------------------------------------------------------------------------
// grammar symbols
// -----------------------------------------------------------------------------
sealed trait Symbol extends SpecElem
object Symbol extends Parser.From[Symbol]

/** terminal symbols */
case class Terminal(term: String) extends Symbol

/** nonterminal symbols */
case class Nonterminal(name: String, args: List[NtArg], optional: Boolean)
  extends Symbol

/** butnot symbols */
case class ButNot(base: Nonterminal, cases: List[Symbol]) extends Symbol

/** but-only-if symbols */
case class ButOnlyIf(base: Nonterminal, methodName: String, cond: String)
  extends Symbol

/** lookahead symbols */
case class Lookahead(contains: Boolean, cases: List[List[Symbol]])
  extends Symbol

/** empty symbols */
case object Empty extends Symbol

/** no-line-terminator symbols */
case object NoLineTerminator extends Symbol

/** symbols for code point abbreviations */
case class CodePointAbbr(abbr: String) extends Symbol

/** symbols for sets of unicode code points with a condition */
case class UnicodeSet(cond: Option[String]) extends Symbol

// -----------------------------------------------------------------------------
// nonterminal arguments
// -----------------------------------------------------------------------------
case class NtArg(kind: NtArg.Kind, name: String) extends SpecElem
object NtArg:
  enum Kind extends SpecElem:
    case True, False, Pass
