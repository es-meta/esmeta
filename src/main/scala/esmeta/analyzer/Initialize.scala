package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.Global
import esmeta.js.Ast
import esmeta.js.builtin.SOURCE_TEXT

/** abstract semantics initializer */
object Initialize {

  /** initialize JavaScript analysis */
  def initJs(sourceText: String): Map[NodePoint[Node], AbsState] = {
    // initial control point
    val initCp = {
      val runJobs = cfg.fnameMap("RunJobs")
      val entry = runJobs.entry.get
      NodePoint(runJobs, entry, View())
    }
    Map(
      initCp -> AbsState.Empty.defineGlobal(
        Global(SOURCE_TEXT) -> AbsValue(sourceText),
      ),
    )
  }

  /** initialize type analysis */
  def initType: Map[NodePoint[Node], AbsState] = ???
  // // TODO remove
  // def typeAnalysisTest(): AbsSemantics = {
  //   val initCp = {
  //     val toBoolean = cfg.fnameMap("ToBoolean")
  //     val entry = toBoolean.entry.get
  //     NodePoint(toBoolean, entry, View())
  //   }
  //   AbsSemantics(
  //     npMap = Map(
  //       initCp -> AbsState.Empty.defineLocal(
  //         Name("argument") -> AbsValue.undef,
  //       ),
  //     ),
  //   )
}
