package esmeta.spec

import esmeta.LINE_SEP
import esmeta.util.Appender

/** stringifier for specifications */
object Stringifier {
  import Appender.*

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
      case elem: Param           => paramRule(app, elem)
      case elem: Param.Kind      => paramKindRule(app, elem)
    }

  // for specifications
  given specRule: Rule[Spec] = (app, spec) => {
    import Production.Kind.*
    val Spec(version, grammar, algorithms) = spec
    val Grammar(prods, prodsForWeb) = grammar
    val prodsBy = prods.groupBy(_.kind)
    version.map(app >> "* version: " >> _ >> LINE_SEP)
    app >> "* grammar:"
    app :> "  - productions: " >> prods.length
    app :> "    - lexical: " >> prodsBy(Lexical).length
    app :> "    - numeric string: " >> prodsBy(NumericString).length
    app :> "    - syntactic: " >> prodsBy(Normal).length
    app :> "  - extended productions for web: " >> grammar.prodsForWeb.length
    app :> "* algorithms:"
    app :> "  - incomplete: " >> "..." // TODO
    app :> "  - complete: " >> "..." // TODO
    app :> "  - total: " >> "..." // TODO
    app :> "* algorithm steps:"
    app :> "  - incompleted: " >> "..." // TODO
    app :> "  - total: " >> "..." // TODO
  }

  // for grammars
  given grammarRule: Rule[Grammar] = (app, grammar) => {
    given Rule[List[Production]] = iterableRule(sep = LINE_SEP)
    app >> "// Productions"
    app :> grammar.prods
    app :> "// Productions for Web"
    app :> grammar.prodsForWeb
  }

  // for productions
  given prodRule: Rule[Production] = (app, prod) => {
    val Production(lhs, kind, oneof, rhsList) = prod
    app >> lhs >> " " >> kind >> (if (oneof) " one of" else "")
    app.wrap("", "")(for (rhs <- rhsList) app :> rhs)
  }

  // for production left-hand-sides (LHSs)
  given lhsRule: Rule[Lhs] = (app, lhs) => {
    val Lhs(name, params) = lhs
    given Rule[List[String]] = iterableRule("[", ", ", "]")
    app >> name >> params
  }

  // for production kinds
  given prodKindRule: Rule[Production.Kind] = (app, kind) => {
    import Production.Kind.*
    app >> (kind match {
      case Normal        => ":"
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
    import Symbol.*
    given n: Rule[List[NtArg]] = iterableRule("[", ", ", "]")
    given t: Rule[List[Symbol]] = iterableRule(sep = " ")
    given ts: Rule[List[List[Symbol]]] = iterableRule("{", ", ", "}")
    symbol match {
      case Terminal(term) => app >> s"`$term`"
      case Nonterminal(name, args, opt) =>
        app >> name
        if (!args.isEmpty) app >> args
        if (opt) app >> "?" else app
      case ButNot(base, cases) =>
        app >> base >> " but not " >> cases
      case Lookahead(b, cases) =>
        app >> "[lookahead " >> (if (b) "<" else "<!") >> " " >> cases >> "]"
      case Empty                       => app >> "[empty]"
      case NoLineTerminator            => app >> "[no LineTerminator here]"
      case Unicode(code)               => app >> "<" >> code >> ">"
      case UnicodeAny                  => app >> "<UnicodeAny>"
      case UnicodeIdStart              => app >> "<UnicodeIdStart>"
      case UnicodeIdContinue           => app >> "<UnicodeIdContinue>"
      case UnicodeLeadSurrogate        => app >> "<UnicodeLeadSurrogate>"
      case UnicodeTrailSurrogate       => app >> "<UnicodeTrailSurrogate>"
      case NotCodePoint                => app >> "<NotCodePoint>"
      case CodePoint                   => app >> "<CodePoint>"
      case HexLeadSurrogate            => app >> "<HexLeadSurrogate>"
      case HexTrailSurrogate           => app >> "<HexTrailSurrogate>"
      case HexNonSurrogate             => app >> "<HexNonSurrogate>"
      case NonUnicodeModeDecimalEscape => app >> "<NonUnicodeModeDecimalEscape>"
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
  given headRule: Rule[Head] = (app, head) => ???

  // for algorithm parameters
  given paramRule: Rule[Param] = (app, param) => ???

  // for algorithm parameter kinds
  given paramKindRule: Rule[Param.Kind] = (app, param) => ???
}
