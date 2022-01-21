package esmeta.cfg

import esmeta.util.BasicParsers

/** CFG parsers */
trait Parsers extends BasicParsers {
  // programs
  given program: Parser[Program] = ???

  // instructions
  given inst: Parser[Inst] = ???

  // expressions
  given expr: Parser[Expr] = ???

  // references
  given ref: Parser[Ref] = ???

  // unary operators
  given uop: Parser[UOp] = ???

  // binary operators
  given bop: Parser[BOp] = ???
}
