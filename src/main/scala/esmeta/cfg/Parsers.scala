package esmeta.cfg

import esmeta.util.BasicParsers

/** CFG parsers */
trait Parsers extends BasicParsers {
  // control flow graphs (CFGs)
  given cfg: Parser[CFG] = ???

  // functions
  given func: Parser[Func] = ???

  // function parameters
  given param: Parser[Param] = ???

  // nodes
  given node: Parser[Node] = ???

  // instructions
  given inst: Parser[Inst] = ???

  // expressions
  given expr: Parser[Expr] = ???

  // unary operators
  given uop: Parser[UOp] = ???

  // binary operators
  given bop: Parser[BOp] = ???

  // conversion operators
  given cop: Parser[COp] = ???

  // references
  given ref: Parser[Ref] = ???

  // identifiers
  val id: Parser[Id] = ???

  // types
  given ty: Parser[Type] = ???
}
