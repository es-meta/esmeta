package esmeta.ir

import esmeta.ir.util.{Parser, YetCollector}
import esmeta.util.UId
import esmeta.spec.{Algorithm, Head}

/** IR functions */
case class Func(
  main: Boolean,
  kind: FuncKind,
  name: String,
  params: List[Param],
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
object Func extends Parser.From(Parser.func)

/** function kinds */
enum FuncKind extends IRElem:
  case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont,
  BuiltinClo
object FuncKind extends Parser.From(Parser.funcKind)
