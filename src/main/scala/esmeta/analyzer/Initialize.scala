package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.Global
import esmeta.js.Ast
import esmeta.js.builtin.SOURCE_TEXT

object Initialize {
  def apply(sourceText: String): Map[NodePoint[Node], AbsState] = {
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
}
