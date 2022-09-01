package esmeta.spec

import esmeta.spec.util.*

/** symbols in ECMAScript grammars */
sealed trait Symbol extends SpecElem {

  /** get an non-terminal or nothing from a symbol */
  def getNt: Option[Nonterminal] = this match
    case (nt: Nonterminal)     => Some(nt)
    case ButNot(base, _)       => Some(base)
    case ButOnlyIf(base, _, _) => Some(base)
    case _                     => None
}
object Symbol extends Parser.From(Parser.symbol)

/** terminal symbols */
case class Terminal(term: String) extends Symbol

/** nonterminal symbols */
case class Nonterminal(
  name: String,
  args: List[NonterminalArgument],
  optional: Boolean,
) extends Symbol
case class NonterminalArgument(
  kind: NonterminalArgumentKind,
  name: String,
) extends SpecElem
object NonterminalArgument extends Parser.From(Parser.ntArg)
enum NonterminalArgumentKind extends SpecElem { case True, False, Pass }
object NonterminalArgumentKind extends Parser.From(Parser.ntArgKind)

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
