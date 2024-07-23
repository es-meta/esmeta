package esmeta.ir

import esmeta.ir.util.Parser

// IR references
sealed trait Ref extends IRElem with LangEdge
object Ref extends Parser.From(Parser.ref)

case class Field(ref: Ref, expr: Expr) extends Ref

sealed trait Var extends Ref
case class Global(name: String) extends Var

sealed trait Local extends Var
case class Name(name: String) extends Local
case class Temp(idx: Int) extends Local

/** ordering of global identifiers */
given Ordering[Global] = Ordering.by(_.name)

/** ordering of local identifiers */
given Ordering[Local] = Ordering.by(_ match
  case Name(name) => (-1, name)
  case Temp(idx)  => (idx, ""),
)
