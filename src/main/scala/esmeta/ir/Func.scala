package esmeta.ir

import esmeta.ir.util.{Parser, YetCollector}
import esmeta.util.UId
import esmeta.spec.{Algorithm, Head}

/** IR functions */
case class Func(
  main: Boolean,
  kind: Func.Kind,
  name: String,
  params: List[Func.Param],
  retTy: Type,
  body: Inst,
  algo: Option[Algorithm] = None,
) extends IRElem {

  /** algorithm heads */
  lazy val yets: List[EYet] = YetCollector(this)
  lazy val complete: Boolean = yets.isEmpty

  /** algorithm heads */
  lazy val head: Option[Head] = algo.map(_.head)

  /** normalized function name */
  def normalizedName: String = name.replace("/", "").replace("`", "")
}
object Func extends Parser.From[Func] {

  /** function kinds */
  enum Kind extends IRElem:
    case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont,
    BuiltinClo
  object Kind extends Parser.From[Kind]

  /** function parameters */
  case class Param(
    lhs: Name,
    optional: Boolean = false,
    ty: Type = Type("Any"),
  ) extends IRElem
  object Param extends Parser.From[Param]
}
