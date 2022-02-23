package esmeta.ir

import esmeta.ir.util.Parser
import esmeta.util.UId
import esmeta.spec.Algorithm

/** IR functions */
case class Func(
  main: Boolean,
  kind: Func.Kind,
  name: String,
  params: List[Func.Param],
  body: Inst,
  algo: Option[Algorithm] = None,
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
}
