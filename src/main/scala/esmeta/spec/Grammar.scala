package esmeta.spec

/** grammars */
case class Grammar(prods: List[Production])

/** productions */
case class Production(
  lhs: Lhs,
  kind: ProductionKind,
  oneof: Boolean,
  rhsList: List[Rhs],
)

/** production kinds */
enum ProductionKind:
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

  /** onlyif symbols */
  case OnlyIf(base: Nonterminal, msg: String) // TODO more detail

  /** lookahead symbols */
  case Lookahead(contains: Boolean, cases: List[List[Symbol]])

  /** empty symbols */
  case EmptySymbol

  /** no-line-terminator symbols */
  case NoLineTerminatorSymbol

  /** unicode symbols */
  case Unicode(code: String) extends Symbol with CharacterSymbol

  /** any unicode symbols */
  case UnicodeAny extends Symbol with CharacterSymbol

  /** ID_Start unicode symbols */
  case UnicodeIdStart extends Symbol with CharacterSymbol

  /** ID_Continue unicode symbols */
  case UnicodeIdContinue extends Symbol with CharacterSymbol

  /** LeadSurrogate unicode symbols */
  case UnicodeLeadSurrogate extends Symbol with CharacterSymbol

  /** TrailSurrogate unicode symbols */
  case UnicodeTrailSurrogate extends Symbol with CharacterSymbol

  /** NotCodePoint symbols */
  case NotCodePoint extends Symbol with CharacterSymbol

  /** CodePoint symbols */
  case CodePoint extends Symbol with CharacterSymbol

  /** HexLeadSurrogate symbols */
  case HexLeadSurrogate extends Symbol with CharacterSymbol

  /** HexTrailSurrogate symbols */
  case HexTrailSurrogate extends Symbol with CharacterSymbol

  /** HexNonSurrogate symbols */
  case HexNonSurrogate extends Symbol with CharacterSymbol

/** character symbols */
trait CharacterSymbol { this: Symbol => }

/** nonterminal arguments */
case class NtArg(kind: NtArgKind, name: String)
enum NtArgKind:
  case True, False, Pass
