package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.{CFG, Node}
import esmeta.error.*
import esmeta.error.NotSupported.given
import esmeta.es.Initialize
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

/** exploded */
def exploded(msg: String): Nothing = throw AnalysisImprecise(msg)

/** not supported */
def notSupported(msg: String): Nothing = throw NotSupported(msg)

/** shortcurts */
val T = Bool(true)
val F = Bool(false)
