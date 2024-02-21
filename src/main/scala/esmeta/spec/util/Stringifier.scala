package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.lang.*
import esmeta.spec.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.Appender
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for specifications */
object Stringifier {
  // stringifier for metalanguage
  val langStringifier = LangElem.getStringifier(false, false)
  import langStringifier.{given, *}
  import TyStringifier.{*, given}

  type NtArg = NonterminalArgument
  type NtArgKind = NonterminalArgumentKind

  given elemRule: Rule[SpecElem] = (app, elem) =>
    elem match
      case elem: Spec           => specRule(app, elem)
      case elem: Summary        => summaryRule(app, elem)
      case elem: Grammar        => grammarRule(app, elem)
      case elem: Production     => prodRule(app, elem)
      case elem: Lhs            => lhsRule(app, elem)
      case elem: ProductionKind => prodKindRule(app, elem)
      case elem: Rhs            => rhsRule(app, elem)
      case elem: RhsCond        => rhsCondRule(app, elem)
      case elem: Symbol         => symbolRule(app, elem)
      case elem: NtArg          => ntArgRule(app, elem)
      case elem: NtArgKind      => ntArgKindRule(app, elem)
      case elem: Algorithm      => algoRule(app, elem)
      case elem: Head           => headRule(app, elem)
      case elem: SdoHeadTarget  => sdoHeadTargetRule(app, elem)
      case elem: BuiltinPath    => builtinPathRule(app, elem)
      case elem: Param          => paramRule(app, elem)
      case elem: ParamKind      => paramKindRule(app, elem)
      case elem: Table          => tableRule(app, elem)

  // for specifications
  given specRule: Rule[Spec] = (app, spec) =>
    if (!spec.isEmpty) app >> spec.summary else app

  // for specification summaries
  given summaryRule: Rule[Summary] = (app, summary) =>
    import ProductionKind.*
    val Summary(version, grammar, algos, steps, types, tables, tyModel) =
      summary
    version.map(app >> "- version: " >> _.toString >> LINE_SEP)
    app >> "- grammar:"
    app :> "  - productions: " >> grammar.productions
    app :> "    - lexical: " >> grammar.lexical
    app :> "    - numeric string: " >> grammar.numeric
    app :> "    - syntactic: " >> grammar.syntactic
    app :> "  - extended productions for web: " >> grammar.web
    app :> "- algorithms: " >> algos.total >> " " >> algos.ratioString
    app :> "  - complete: " >> algos.complete
    app :> "  - incomplete: " >> algos.incomplete
    app :> "- algorithm steps: " >> steps.total >> " " >> steps.ratioString
    app :> "  - complete: " >> steps.complete
    app :> "  - incomplete: " >> steps.incomplete
    app :> "- types: " >> types.total >> " " >> types.ratioString
    app :> "  - known: " >> types.known
    app :> "  - yet: " >> types.yet
    app :> "  - unknown: " >> types.unknown
    app :> "- tables: " >> tables
    app :> "- type model: " >> tyModel

  // for grammars
  given grammarRule: Rule[Grammar] = (app, grammar) =>
    given Rule[List[Production]] = iterableRule(sep = LINE_SEP * 2)
    app >> "<Productions>"
    app :> grammar.prods
    app :> ""
    app :> "<Productions for Web>"
    app :> grammar.prodsForWeb

  // for productions
  given prodRule: Rule[Production] = (app, prod) =>
    val Production(lhs, kind, oneof, rhsVec) = prod
    app >> lhs >> " " >> kind
    given Rule[Iterable[Rhs]] = iterableRule(sep = " ")
    if (oneof) app.wrap(" one of", "")(app :> rhsVec)
    else app.wrap("", "")(for (rhs <- rhsVec) app :> rhs)

  // for production left-hand-sides (LHSs)
  given lhsRule: Rule[Lhs] = (app, lhs) =>
    val Lhs(name, params) = lhs
    given Rule[List[String]] = iterableRule("[", ", ", "]")
    app >> name
    if (!params.isEmpty) app >> params else app

  // for ProductionKinds
  given prodKindRule: Rule[ProductionKind] = (app, kind) =>
    import ProductionKind.*
    app >> (kind match
      case Syntactic     => ":"
      case Lexical       => "::"
      case NumericString => ":::"
    )

  // for production alternative right-hand-sides (RHSs)
  given rhsRule: Rule[Rhs] = (app, rhs) =>
    val Rhs(conditions, symbols, id) = rhs
    given Rule[List[Symbol]] = iterableRule(sep = " ")
    if (conditions.nonEmpty) app >> conditions >> " "
    app >> symbols
    id.foreach(app >> " #" >> _)
    app

  // for list of conditions for RHSs
  given rhsCondsRule: Rule[List[RhsCond]] = iterableRule("[", ", ", "]")

  // for conditions for RHSs
  given rhsCondRule: Rule[RhsCond] = (app, rhsCond) =>
    val RhsCond(name, pass) = rhsCond
    app >> (if (pass) "+" else "~") >> name

  // for conditions for symbols
  given symbolRule: Rule[Symbol] = (app, symbol) =>
    given n: Rule[List[NtArg]] = iterableRule("[", ", ", "]")
    given t: Rule[List[Symbol]] = iterableRule(sep = " ")
    given ts: Rule[List[List[Symbol]]] = iterableRule("{", ", ", "}")
    symbol match
      case Terminal(term)      => app >> s"`$term`"
      case ButNot(base, cases) => app >> base >> " but not " >> cases
      case Empty               => app >> "[empty]"
      case NoLineTerminator    => app >> "[no LineTerminator here]"
      case CodePoint(cp, desc) =>
        app >> "<U+" >> cp >> (if (desc == "") "" else " " + desc) >> ">"
      case CodePointAbbr(abbr) => app >> "<" >> abbr >> ">"
      case Nonterminal(name, args) =>
        app >> name
        if (!args.isEmpty) app >> args else app
      case Optional(symbol) =>
        app >> symbol >> "?"
      case Lookahead(b, cases) =>
        app >> "[lookahead " >> (if (b) "<" else "<!") >> " " >> cases >> "]"
      case ButOnlyIf(base, name, cond) =>
        app >> base >> " [> but only if " >> name >> " of "
        app >> "|" >> base.name >> "|" >> cond >> "]"
      case UnicodeSet(cond) =>
        app >> "> any Unicode code point"
        cond.map(app >> " " >> _)
        app

  // for conditions for nonterminal arguments
  given ntArgRule: Rule[NtArg] = (app, ntArg) =>
    val NtArg(kind, name) = ntArg
    app >> kind >> name

  // for conditions for nonterminal argument kinds
  given ntArgKindRule: Rule[NtArgKind] = (app, kind) =>
    import NonterminalArgumentKind.*
    app >> (kind match
      case True  => "+"
      case False => "~"
      case Pass  => "?"
    )

  // for algorithms
  given algoRule: Rule[Algorithm] = (app, algo) => ??? // TODO

  // for algorithm heads
  given headRule: Rule[Head] = (app, head) =>
    given Rule[List[Param]] = iterableRule("(", ", ", ")")
    head match
      case AbstractOperationHead(isHostDefined, name, params, rty) =>
        app >> name >> params >> ": " >> rty
      case NumericMethodHead(ty, name, params, rty) =>
        app >> ty.ty >> "::" >> name >> params >> ": " >> rty
      case SyntaxDirectedOperationHead(
            target,
            methodName,
            isStatic,
            withParams,
            rty,
          ) =>
        given Rule[Option[SdoHeadTarget]] = optionRule("<DEFAULT>")
        app >> "[SYNTAX] " >> target >> "." >> methodName
        if (isStatic) app >> "[" >> "S" >> "]"
        else app >> "[" >> "R" >> "]"
        app >> withParams >> ": " >> rty
      case ConcreteMethodHead(methodName, receiverParam, params, rty) =>
        app >> "[METHOD] " >> methodName >> "(" >> receiverParam.name >> ")"
        app >> params >> ": " >> rty
      case InternalMethodHead(methodName, receiverParam, params, rty) =>
        app >> "[METHOD] " >> methodName >> "(" >> receiverParam.name >> ")"
        app >> params >> ": " >> rty
      case BuiltinHead(path, params, rty) =>
        app >> "[BUILTIN] " >> path >> params >> ": " >> rty

  given builtinPathRule: Rule[BuiltinPath] = (app, path) =>
    import BuiltinPath.*
    path match
      case Base(name)               => app >> name
      case NormalAccess(base, name) => app >> base >> "." >> name
      case Getter(base)             => app >> "get " >> base
      case Setter(base)             => app >> "set " >> base
      case SymbolAccess(base, symbol) =>
        app >> base >> "[@@" >> symbol >> "]"
      case YetPath(name) => app >> "yet:" >> name.replace(" ", "")

  // for syntax-directed operation head targets
  given sdoHeadTargetRule: Rule[SdoHeadTarget] = (app, target) =>
    given Rule[List[Param]] = iterableRule("(", ", ", ")")
    val SdoHeadTarget(lhsName, idx, subIdx, rhsParams) = target
    app >> lhsName >> "[" >> idx >> ", " >> subIdx >> "]" >> rhsParams

  // TODO: for algorithm parameters
  given paramRule: Rule[Param] = (app, param) => app >> param.name

  // TODO: for algorithm parameter kinds
  given paramKindRule: Rule[ParamKind] = (app, param) => ???

  // TODO: for tables
  given tableRule: Rule[Table] = (app, table) => ???
}
