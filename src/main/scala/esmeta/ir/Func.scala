package esmeta.ir

import esmeta.ir.util.{Parser, YetCollector, Stringifier}
import esmeta.util.UId
import esmeta.spec.{Algorithm, Head}
import esmeta.util.Appender

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

  /** not yet supported instructions */
  lazy val yets: List[EYet] = YetCollector(this)

  /** check completeness */
  lazy val complete: Boolean = yets.isEmpty

  /** algorithm heads */
  lazy val head: Option[Head] = algo.map(_.head)

  /** algorithm head string */
  def headString: String =
    val app = new Appender
    IRElem.getStringifier(true, false).funcHeadRule(true)(app, this)
    app.toString

  /** normalized function name */
  def normalizedName: String = name.replace("/", "").replace("`", "")
}
object Func extends Parser.From(Parser.func)

/** function kinds */
enum FuncKind extends IRElem:
  case AbsOp, NumMeth, SynDirOp, ConcMeth, InternalMeth, Builtin, Clo, Cont,
  BuiltinClo
object FuncKind extends Parser.From(Parser.funcKind)
