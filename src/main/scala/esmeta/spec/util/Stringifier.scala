package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.util.Appender
import esmeta.util.BaseUtils.*
import esmeta.spec.*

/** stringifier for specifications */
object Stringifier {
  import Appender.*, Symbol.*

  given elemRule: Rule[SpecElem] = (app, elem) =>
    elem match {
      case elem: Spec            => specRule(app, elem)
      case elem: Grammar         => grammarRule(app, elem)
      case elem: Production      => prodRule(app, elem)
      case elem: Lhs             => lhsRule(app, elem)
      case elem: Production.Kind => prodKindRule(app, elem)
      case elem: Rhs             => rhsRule(app, elem)
      case elem: RhsCond         => rhsCondRule(app, elem)
      case elem: Symbol          => symbolRule(app, elem)
      case elem: NtArg           => ntArgRule(app, elem)
      case elem: NtArg.Kind      => ntArgKindRule(app, elem)
      case elem: Algorithm       => algoRule(app, elem)
      case elem: Head            => headRule(app, elem)
      case elem: SdoHeadTarget   => sdoHeadTargetRule(app, elem)
      case elem: Param           => paramRule(app, elem)
      case elem: Param.Kind      => paramKindRule(app, elem)
      case elem: Table           => tableRule(app, elem)
    }

  // for specifications
  given specRule: Rule[Spec] = (app, spec) => {
    import Production.Kind.*
    val Spec(version, grammar, algorithms, tables, typeModel, _) = spec
    val Grammar(prods, prodsForWeb) = grammar
    val prodsBy = prods.groupBy(_.kind)
    version.map(app >> "* version: " >> _ >> LINE_SEP)
    app >> "* grammar:"
    app :> "  - productions: " >> prods.length
    app :> "    - lexical: " >> prodsBy(Lexical).length
    app :> "    - numeric string: " >> prodsBy(NumericString).length
    app :> "    - syntactic: " >> prodsBy(Syntactic).length
    app :> "  - extended productions for web: " >> grammar.prodsForWeb.length

    val (algoPass, algoTotal) =
      (spec.completeAlgorithms.length, algorithms.length)
    val algoRatio = ratioSimpleString(algoPass, algoTotal)
    app :> "* algorithms: " >> algoTotal >> " " >> algoRatio
    app :> "  - complete: " >> algoPass
    app :> "  - incomplete: " >> algoTotal - algoPass

    val (stepPass, stepTotal) =
      (spec.completeSteps.length, spec.allSteps.length)
    val stepRatio = ratioSimpleString(stepPass, stepTotal)
    app :> "* algorithm steps: " >> stepTotal >> " " >> stepRatio
    app :> "  - complete: " >> stepPass
    app :> "  - incompleted: " >> stepTotal - stepPass

    app :> "* tables: " >> tables.size

    app :> "* type model: " >> typeModel.infos.size
  }

  // for grammars
  given grammarRule: Rule[Grammar] = (app, grammar) => {
    given Rule[List[Production]] = iterableRule(sep = LINE_SEP * 2)
    app >> "// Productions"
    app :> grammar.prods
    app :> ""
    app :> "// Productions for Web"
    app :> grammar.prodsForWeb
  }

  // for productions
  given prodRule: Rule[Production] = (app, prod) => {
    val Production(lhs, kind, oneof, rhsList) = prod
    app >> lhs >> " " >> kind
    given Rule[List[Rhs]] = iterableRule(sep = " ")
    if (oneof) app.wrap(" one of", "")(app :> rhsList)
    else app.wrap("", "")(for (rhs <- rhsList) app :> rhs)
  }

  // for production left-hand-sides (LHSs)
  given lhsRule: Rule[Lhs] = (app, lhs) => {
    val Lhs(name, params) = lhs
    given Rule[List[String]] = iterableRule("[", ", ", "]")
    app >> name
    if (!params.isEmpty) app >> params else app
  }

  // for production kinds
  given prodKindRule: Rule[Production.Kind] = (app, kind) => {
    import Production.Kind.*
    app >> (kind match {
      case Syntactic     => ":"
      case Lexical       => "::"
      case NumericString => ":::"
    })
  }

  // for production alternative right-hand-sides (RHSs)
  given rhsRule: Rule[Rhs] = (app, rhs) => {
    val Rhs(condition, symbols, id) = rhs
    given Rule[List[Symbol]] = iterableRule(sep = " ")
    condition.foreach(app >> _ >> " ")
    app >> symbols
    id.foreach(app >> " #" >> _)
    app
  }

  // for condidtions for RHSs
  given rhsCondRule: Rule[RhsCond] = (app, rhsCond) => {
    val RhsCond(name, pass) = rhsCond
    app >> "[" >> (if (pass) "+" else "~") >> name >> "]"
  }

  // for condidtions for symbols
  given symbolRule: Rule[Symbol] = (app, symbol) =>
    given n: Rule[List[NtArg]] = iterableRule("[", ", ", "]")
    given t: Rule[List[Symbol]] = iterableRule(sep = " ")
    given ts: Rule[List[List[Symbol]]] = iterableRule("{", ", ", "}")
    symbol match {
      case Terminal(term)      => app >> s"`$term`"
      case ButNot(base, cases) => app >> base >> " but not " >> cases
      case Empty               => app >> "[empty]"
      case NoLineTerminator    => app >> "[no LineTerminator here]"
      case CodePointAbbr(abbr) => app >> "<" >> abbr >> ">"
      case Nonterminal(name, args, opt) =>
        app >> name
        if (!args.isEmpty) app >> args
        if (opt) app >> "?" else app
      case Lookahead(b, cases) =>
        app >> "[lookahead " >> (if (b) "<" else "<!") >> " " >> cases >> "]"
      case ButOnlyIf(base, name, cond) =>
        app >> base >> " [> but only if " >> name >> " of "
        app >> "|" >> base.name >> "|" >> cond >> "]"
      case UnicodeSet(cond) =>
        app >> "> any Unicode code point"
        cond.map(app >> " " >> _)
        app
    }

  // for condidtions for nonterminal arguments
  given ntArgRule: Rule[NtArg] = (app, ntArg) => {
    val NtArg(kind, name) = ntArg
    app >> kind >> name
  }

  // for condidtions for nonterminal argument kinds
  given ntArgKindRule: Rule[NtArg.Kind] = (app, kind) =>
    import NtArg.Kind.*
    app >> (kind match {
      case True  => "+"
      case False => "~"
      case Pass  => "?"
    })

  // for algorithms
  given algoRule: Rule[Algorithm] = (app, algo) => ???

  // for algorithm heads
  given headRule: Rule[Head] = (app, head) =>
    given Rule[List[Param]] = iterableRule("(", ", ", ")")
    head match {
      case AbstractOperationHead(name, params, isHostDefined) =>
        app >> name >> params
      case NumericMethodHead(ty, name, params) =>
        app >> ty >> "::" >> name >> params
      case SyntaxDirectedOperationHead(
            target,
            methodName,
            isStatic,
            withParams,
          ) =>
        given Rule[Option[SdoHeadTarget]] = optionRule("<DEFAULT>")
        app >> "[SYNTAX] " >> target >> "." >> methodName
        if (isStatic) app >> "[" >> "S" >> "]"
        else app >> "[" >> "R" >> "]"
        app >> withParams
      case ConcreteMethodHead(methodName, receiverParam, params) =>
        app >> "[METHOD] " >> methodName >> "(" >> receiverParam.name >> ")" >> params
      case InternalMethodHead(methodName, receiverParam, params) =>
        app >> "[METHOD] " >> methodName >> "(" >> receiverParam.name >> ")"
        app >> params
      case BuiltinHead(ref, params) =>
        app >> "[BUILTIN] " >> ref >> params
    }

  // for syntax-directed operation head targets
  given sdoHeadTargetRule: Rule[SdoHeadTarget] = (app, target) => {
    given Rule[List[Param]] = iterableRule("(", ", ", ")")
    val SdoHeadTarget(lhsName, idx, subIdx, rhsParams) = target
    app >> lhsName >> "[" >> idx >> ", " >> subIdx >> "]" >> rhsParams
  }

  // TODO: for algorithm parameters
  given paramRule: Rule[Param] = (app, param) => app >> param.name

  // TODO: for algorithm parameter kinds
  given paramKindRule: Rule[Param.Kind] = (app, param) => ???

  // TODO: for tables
  given tableRule: Rule[Table] = (app, table) => ???
}
