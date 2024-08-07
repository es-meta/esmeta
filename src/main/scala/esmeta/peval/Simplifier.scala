package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.{Expr, Func, Inst, Program, Ref}
import esmeta.ir.*
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

/** IR Func simplifier using use-def and escaping object information */
class Simplifier(
  val pst: PState,
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  // TODO
  /** resulting function */
  lazy val result: Func = targetFunc

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/simplify_log"))

}

object Simplifier {
  def apply(
    prog: Program,
    log: Boolean = false,
    output: Option[String] = None,
    logPW: Option[PrintWriter] = None,
  ): Program =
    Program(
      funcs = prog.funcs.map(func =>
        new Simplifier(
          PState(func),
          prog,
          func,
          log,
          output,
          logPW,
        ).result,
      ),
      spec = prog.spec,
    )
}
