package esmeta.spec.parsers

import esmeta.spec.*

/** parsers for productions */
trait ProductionParsers extends RhsParsers {
  import Production.Kind.*

  // productions
  lazy val prods: Parser[List[Production]] =
    rep1(rep(newline) ~> prod)

  // productions
  lazy val prod: Parser[Production] =
    lhs ~ prodKind ~ opt("one of") ~ rep1(opt(newline) ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs =
          for (r <- origRs; s <- r.symbols)
            yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs =>
        Production(l, k, false, rs)
    }

  // production kinds
  lazy val prodKind: Parser[Production.Kind] =
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Normal

  // production left-hand-sides (LHSs)
  lazy val lhs: Parser[Lhs] =
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ { case name ~ params =>
      Lhs(name, params.getOrElse(Nil))
    }
}
