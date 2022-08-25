package esmeta.ai.util

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ir.IRElem
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for ai */
class Stringifier(
  detail: Boolean,
  location: Boolean,
  asite: Boolean,
) {
  private val cfgStringifier = CFGElem.getStringifier(detail, location)
  import cfgStringifier.given

  private val irStringifier = IRElem.getStringifier(detail, location)
  import irStringifier.given

  /** elements */
  given elemRule: Rule[AnalyzerElem] = (app, elem) => ???

  /** view */
  given viewRule: Rule[View] = (app, view) => ???

  // control points
  given cpRule: Rule[ControlPoint] = (app, cp) => ???

  // values for analysis
  given avRule: Rule[AValue] = (app, av) => ???

  // TODO type
  // given typeRule: Rule[Type] = (app, ty) => ???
}
