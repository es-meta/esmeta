package esmeta.lang

/** TODO statements */
enum Stmt extends LangElem:
  case Block(stmts: List[Stmt])

/** TODO references */
trait Ref
