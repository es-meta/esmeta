package esmeta.spec.util

import esmeta.spec.*
import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.{Program => IRProgram}
import esmeta.lang.{Step, YetStep}
import esmeta.lang.util.StepCollector
import esmeta.{EXTRACT_LOG_DIR, LINE_SEP}
import esmeta.util.BaseUtils.{cached, time}
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap}

/** extensions for Elements */
extension (elem: Element) {

  /** walker for ancestors */
  def walkAncestor[T](
    f: Element => T,
    base: T,
    join: (T, T) => T,
  ): T =
    val parent = elem.parent
    if (parent == null) base
    else join(f(parent), parent.walkAncestor(f, base, join))

  /** checks whether an element is in appendix */
  def isInAnnex: Boolean =
    elem.walkAncestor(_.tagName == "emu-annex", false, _ || _)

  /** checks whether an element is of Chapter 5. Notational Conventions */
  def isNotation: Boolean =
    elem.parent match {
      case null => false
      case parent =>
        if (parent.id == "sec-notational-conventions") true
        else parent.isNotation
    }
}
