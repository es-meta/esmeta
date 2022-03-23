package esmeta.interp.util

import esmeta.interp.*

/** breakpoints used in debugger */
trait Breakpoint {
  var enabled: Boolean
  private var trigger = false
  def needTrigger: Boolean = {
    if (trigger) { trigger = false; true }
    else false
  }
  protected def on: Unit = trigger = true
  def check(st: State): Unit
  def toggle() = { enabled = !enabled }
}
