package esmeta.spec.parsers

import esmeta.spec.*

/** parsers for production alternative right-hand-sides (RHSs) */
trait RhsParsers extends SymbolParsers {
  // production alternative right-hand-sides (RHSs)
  lazy val rhs: Parser[Rhs] =
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ { case c ~ ss ~ i =>
      Rhs(c, ss, i)
    }

  // RHS conditions
  lazy val rhsCond: Parser[RhsCond] =
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ { case str ~ name =>
      RhsCond(name, str == "+")
    }

  // RHS ids
  lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r
}
