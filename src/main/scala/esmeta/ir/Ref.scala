package esmeta.ir

import esmeta.ir.util.*

// IR references
sealed trait Ref extends IRElem
object Ref extends Parser.From[Ref]

case class Prop(ref: Ref, expr: Expr) extends Ref

sealed trait Id extends Ref
case class Global(name: String) extends Id

sealed trait Local extends Id
case class Name(name: String) extends Local
case class Temp(idx: Int) extends Local
