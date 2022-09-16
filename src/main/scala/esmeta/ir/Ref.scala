package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.lang.Syntax

// IR references
sealed trait Ref extends IRElem:
  val langOpt: Option[Syntax]
object Ref extends Parser.From(Parser.ref)

case class Prop(ref: Ref, expr: Expr, langOpt: Option[Syntax] = None)
  extends Ref

sealed trait Id extends Ref
case class Global(name: String, langOpt: Option[Syntax] = None) extends Id

sealed trait Local extends Id
case class Name(name: String, langOpt: Option[Syntax] = None) extends Local
case class Temp(idx: Int, langOpt: Option[Syntax] = None) extends Local

/** ordering of global identifiers */
given Ordering[Global] = Ordering.by(_.name)

/** ordering of local identifiers */
given Ordering[Local] = Ordering.by(local =>
  local match
    case Name(name, _) => (-1, name)
    case Temp(idx, _)  => (idx, ""),
)
