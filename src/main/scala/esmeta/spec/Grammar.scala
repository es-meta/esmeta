package esmeta.spec

/** grammars */
case class Grammar(prods: List[Production])

/** productions */
case class Production(kind: ProductionKind, lhs: Lhs, rhsList: List[Rhs])

/** production kinds */
enum ProductionKind:
  case Normal, Lexical, NumericString

/** production left-hand-sides (LHSs) */
case class Lhs(name: String, params: List[String])

/** production alternative right-hand-sides (RHSs) */
case class Rhs(
  symbols: List[Symbol],
  condition: Option[RhsCond],
  id: Option[String],
)

/** condidtions for RHSs */
case class RhsCond(name: String, pass: Boolean)

/** grammar symbols */
enum Symbol:
  /** terminal symbols */
  case Terminal(term: String)

  /** nonterminal symbols */
  case NonTerminal(name: String, args: List[String], optional: Boolean)

  /** butnot symbols */
  case ButNot(base: Symbol, cases: List[Symbol])

  /** lookahead symbols */
  case Lookahead(contains: Boolean, cases: List[List[Symbol]])

  /** empty symbols */
  case EmptySymbol

  /** no-line-terminator symbols */
  case NoLineTerminatorSymbol

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
