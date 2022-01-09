package esmeta.spec

/** grammars */
case class Grammar(
  prods: List[Production],
  prodsForWeb: List[Production],
)

/** productions */
case class Production(
  lhs: Lhs,
  kind: Production.Kind,
  oneof: Boolean,
  rhsList: List[Rhs],
)
object Production:
  enum Kind:
    case Normal, Lexical, NumericString

/** production left-hand-sides (LHSs) */
case class Lhs(name: String, params: List[String])

/** production alternative right-hand-sides (RHSs) */
case class Rhs(
  condition: Option[RhsCond],
  symbols: List[Symbol],
  id: Option[String],
)

/** condidtions for RHSs */
case class RhsCond(name: String, pass: Boolean)

/** grammar symbols */
enum Symbol:
  /** terminal symbols */
  case Terminal(term: String)

  /** nonterminal symbols */
  case Nonterminal(name: String, args: List[NtArg], optional: Boolean)

  /** butnot symbols */
  case ButNot(base: Nonterminal, cases: List[Symbol])

  /** lookahead symbols */
  case Lookahead(contains: Boolean, cases: List[List[Symbol]])

  /** empty symbols */
  case Empty

  /** no-line-terminator symbols */
  case NoLineTerminator

  /** unicode symbols */
  case Unicode(code: String)

  /** any unicode symbols */
  case UnicodeAny

  /** ID_Start unicode symbols */
  case UnicodeIdStart

  /** ID_Continue unicode symbols */
  case UnicodeIdContinue

  /** LeadSurrogate unicode symbols */
  case UnicodeLeadSurrogate

  /** TrailSurrogate unicode symbols */
  case UnicodeTrailSurrogate

  /** NotCodePoint symbols */
  case NotCodePoint

  /** CodePoint symbols */
  case CodePoint

  /** HexLeadSurrogate symbols */
  case HexLeadSurrogate

  /** HexTrailSurrogate symbols */
  case HexTrailSurrogate

  /** HexNonSurrogate symbols */
  case HexNonSurrogate

  /** NonUnicodeModeDecimalEscape symbols */
  case NonUnicodeModeDecimalEscape

/** nonterminal arguments */
case class NtArg(kind: NtArg.Kind, name: String)
object NtArg:
  enum Kind:
    case True, False, Pass
