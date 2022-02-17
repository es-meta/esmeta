package esmeta.ir

import esmeta.ir.util.*
import esmeta.util.UId

// IR functions
case class Func(
  head: Func.Head,
  body: Inst,
) extends IRElem

object Func extends Parser.From[Func] {

  /** function kinds */
  enum Kind extends IRElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont
  object Kind extends Parser.From[Kind]

  /** function parameters */
  case class Param(
    lhs: Name,
    optional: Boolean = false,
    ty: Type = Type("Any"),
  ) extends IRElem
  object Param extends Parser.From[Param]

  /** function heads */
  case class Head(
    main: Boolean,
    kind: Func.Kind,
    name: String,
    params: List[Func.Param],
  ) extends IRElem
  object Head extends Parser.From[Head]
}
