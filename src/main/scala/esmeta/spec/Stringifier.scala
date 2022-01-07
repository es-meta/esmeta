package esmeta.spec

import esmeta.LINE_SEP
import esmeta.util.Appender

/** stringifier for specifications */
object Stringifier {
  import Utils.*
  import Appender.*

  // for specifications
  given Appendable[Spec] = (app, spec) => ???

  // for grammars
  given Appendable[Grammar] = (app, grammar) => {
    given Appendable[List[Production]] = iterableApp(sep = LINE_SEP)
    app >> getSortedProds(grammar)
  }

  // for productions
  given Appendable[Production] = (app, prod) => {
    val Production(lhs, kind, oneof, rhsList) = prod
    app >> lhs >> " " >> kind >> (if (oneof) " one of" else "")
    app.wrap(for (rhs <- rhsList) app :> rhs)
  }

  // for production left-hand-sides (LHSs)
  given Appendable[Lhs] = (app, lhs) => {
    val Lhs(name, params) = lhs
    given Appendable[List[String]] = iterableApp("[", ", ", "]")
    app >> name >> params
  }

  // for production kinds
  given Appendable[Production.Kind] = (app, kind) => {
    import Production.Kind.*
    app >> (kind match {
      case Normal        => ":"
      case Lexical       => "::"
      case NumericString => ":::"
    })
  }

  // for production alternative right-hand-sides (RHSs)
  given Appendable[Rhs] = (app, rhs) => {
    val Rhs(condition, symbols, id) = rhs
    given Appendable[List[Symbol]] = iterableApp(sep = " ")
    condition.foreach(app >> _ >> " ")
    app >> symbols
    id.foreach(app >> " " >> _)
    app
  }

  // for condidtions for RHSs
  given Appendable[RhsCond] = (app, rhsCond) => {
    val RhsCond(name, pass) = rhsCond
    app >> "[" >> (if (pass) "+" else "~") >> name >> "]"
  }

  // for condidtions for symbols
  given Appendable[Symbol] = (app, symbol) =>
    import Symbol.*
    given n: Appendable[List[NtArg]] = iterableApp("[", ", ", "]")
    given t: Appendable[List[Symbol]] = iterableApp(sep = " ")
    given ts: Appendable[List[List[Symbol]]] = iterableApp("{", ", ", "}")
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
  given Appendable[NtArg] = (app, ntArg) => {
    val NtArg(kind, name) = ntArg
    app >> kind >> name
  }

  // for condidtions for nonterminal argument kinds
  given ntArgKindApp: Appendable[NtArg.Kind] = (app, kind) =>
    import NtArg.Kind.*
    app >> (kind match {
      case True  => "+"
      case False => "~"
      case Pass  => "?"
    })

  // for algorithms
  given Appendable[Algorithm] = (app, algo) => ???

  // for algorithm heads
  given Appendable[Head] = (app, head) => ???

  // for algorithm parameters
  given Appendable[Param] = (app, param) => ???
}
