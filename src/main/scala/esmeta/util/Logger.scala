package esmeta.util

import esmeta.util.BaseUtils

/** logger during analysis */
class Logger {
  // warnings
  private var warnings: Vector[String] = Vector()
  def getWarnings: Vector[String] = warnings
  def warn(msg: String, verbose: Boolean = false): Unit =
    warnings :+= msg; if (verbose) BaseUtils.warn(msg)

  // reset
  def reset: Unit = warnings = Vector()
}
