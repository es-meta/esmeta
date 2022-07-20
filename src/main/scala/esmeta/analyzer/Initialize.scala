package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.js
import esmeta.js.Ast

object Initialize {
  def apply(
    cfg: CFG,
    sourceText: String,
  ): (NodePoint[Node], BasicRetDomain) = {
    // initial control point
    val initCp = {
      val runJobs = cfg.fnameMap("RunJobs")
      val entry = runJobs.entry.get
      NodePoint(runJobs, entry, View())
    }

    // concrete state initializer
    val initializer = new js.Initialize(cfg, sourceText, None)

    // initial abstract domain
    val AbsHeap = BasicHeapDomain(cfg, initializer.initHeap)
    val AbsState = BasicStateDomain(AbsHeap, initializer.initGlobal.toMap)
    val AbsRet = BasicRetDomain(AbsState)

    // result
    (initCp, AbsRet)
  }
}
