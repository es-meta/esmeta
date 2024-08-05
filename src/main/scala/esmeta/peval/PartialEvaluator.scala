package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.Program
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

class PartialEvaluator(
  val program: Program,
  val log: Boolean = false,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting program */
  lazy val result: Program = {
    if (log) pw.print(program)
    pw.close()
    program
  }

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/log"))
}
