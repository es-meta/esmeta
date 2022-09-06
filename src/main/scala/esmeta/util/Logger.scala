package esmeta.util

import esmeta.LINE_SEP

/** logger during analysis */
class Logger {
  // warnings
  private var warnings: Vector[String] = Vector()
  def getWarnings: Vector[String] = warnings
  def warn(msg: String, verbose: Boolean = true): Unit =
    warnings :+= msg; if (verbose) BaseUtils.warn(msg)

  /** conversion to string */
  override def toString: String =
    warnings.map("[WARNING] " + _).mkString(LINE_SEP)

  // reset
  def reset: Unit = warnings = Vector()
}
