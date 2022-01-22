package esmeta.cfg

import esmeta.util.BasicParsers

/** CFG parsers */
trait Parsers extends BasicParsers {
  // control flow graphs (CFGs)
  given cfg: Parser[CFG] = x => ???

  // functions
  given func: Parser[Func] = x => ???

  // function parameters
  given param: Parser[Param] = x => ???

  // nodes
  given node: Parser[Node] = x => ???

  // instructions
  given inst: Parser[Inst] = x => ???

  // expressions
  given expr: Parser[Expr] = x => ???

  // unary operators
  given uop: Parser[UOp] = x => ???

  // binary operators
  given bop: Parser[BOp] = x => ???

  // conversion operators
  given cop: Parser[COp] = x => ???

  // references
  given ref: Parser[Ref] = x => ???

  // identifiers
  val id: Parser[Id] = x => ???

  // types
  given ty: Parser[Type] = x => ???
}
