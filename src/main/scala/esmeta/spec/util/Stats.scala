package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.lang.*
import esmeta.lang.util.CaseCollector
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap, ListBuffer}

/** specification statistics */
class Stats(spec: Spec) {

  /** dump statistics */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    println(s"- Dumped statistics info. into $baseDir")
    dumpFile(ElemStat.getAlgoString, s"$baseDir/algo-summary")
    dumpFile(ElemStat.getStepString, s"$baseDir/step-summary")
    dumpFile(CaseStat.getStepString, s"$baseDir/step-details")
    dumpFile(CaseStat.getExprString, s"$baseDir/expr-details")
    dumpFile(CaseStat.getCondString, s"$baseDir/cond-details")

  /** statistics for pass/total */
  private case class PassStat(pass: Int = 0, total: Int = 0):
    def +(other: PassStat): PassStat = PassStat(
      pass + other.pass,
      total + other.total,
    )
    def isEmpty: Boolean = total == 0
    def fail: Int = total - pass
    override def toString = if (total != 0) ratioString(pass, total) else ""
  private object PassStat:
    def apply(b: Boolean): PassStat = PassStat(if (b) 1 else 0, 1)

  /** statistics for elements in spec */
  object ElemStat {

    /** statistics for each elements */
    private case class ElemStat(
      algo: PassStat = PassStat(),
      step: PassStat = PassStat(),
    ) {
      def +(other: ElemStat): ElemStat = ElemStat(
        algo + other.algo,
        step + other.step,
      )
    }

    /** maps from element to stat */
    private val map: MMap[Element, ElemStat] = MMap()

    /** add algorithm */
    def +=(algo: Algorithm): Unit = {
      val completed = algo.completeSteps.length
      val total = algo.steps.length
      val stat = ElemStat(
        PassStat(completed == total),
        PassStat(completed, total),
      )
      algo.elem.walkAncestor(
        elem =>
          val elemStat = map.getOrElse(elem, ElemStat())
          map += (elem -> (elemStat + stat))
        ,
        (),
        (a, b) => (),
      )
    }

    /** get summary of element */
    private def getElementString(
      elem: Element,
      pstat: ElemStat => PassStat,
      yet: Boolean = false,
      indent: Int = 0,
    ): String = {

      // get new line string
      def newline(indent: Int = 0): String = LINE_SEP + "  " * indent

      // get pass stat
      val stat = pstat(map.getOrElse(elem, ElemStat()))

      // get yet steps
      val printYet = yet && stat.fail != 0 && !elem.children.toList
        .filter(_.tagName == "emu-alg")
        .isEmpty
      val yetStepStr =
        if (printYet)
          // get algos in same emu-clause
          val algos = spec.algorithms.filter(_.elem.getId == elem.id)
          // get yet steps
          algos
            .map(_.incompleteSteps)
            .flatten
            .map(newline(indent + 1) + "1. " + _.toString(false))
            .fold("")(_ + _)
        else ""

      // final result
      if (elem.tagName == "body") stat.toString
      else if (elem.tagName == "emu-clause" && !stat.isEmpty)
        s"${newline(indent)}- ${elem.id}:${stat}${yetStepStr}"
      else ""
    }

    /** getString */
    def getAlgoString: String =
      getString(spec.document.body, elem => elem.algo)
    def getStepString: String =
      getString(spec.document.body, elem => elem.step, true)
    private def getString(
      elem: Element,
      pstat: ElemStat => PassStat,
      yet: Boolean = false,
      indent: Int = 0,
    ): String =
      val elemStr = getElementString(elem, pstat, yet, indent)
      val children = elem.children.toList
      if (!children.isEmpty)
        children
          .map(getString(_, pstat, yet, indent + 1))
          .fold(elemStr)(_ + _)
      else elemStr

  }

  /** statistics for cases in spec */
  object CaseStat {
    import CaseCollector.*

    val collector = new CaseCollector
    import collector.*

    /** add algorithms */
    def +=(algo: Algorithm): Unit = collector.walk(algo.body)

    /** getString */
    def getStepString: String = getString(steps)
    def getExprString: String = getString(exprs)
    def getCondString: String = getString(conds)
    private def getString[T](
      map: MMap[String, MMap[String, ListBuffer[T]]],
    ): String =
      val app = new Appender
      val mapWithCount = map.map { (x, m) => x -> (m.map(_._2.size).sum, m) }
      for { (className, (k, m)) <- getSortedList(mapWithCount, _._1) } {
        app >> f"" >> className >> " (" >> k >> ")"
        for { (str, xs) <- getSortedList(m, _.size) } {
          app :> f"- [${xs.size}%6d] " >> str
        }
        app >> LINE_SEP
      }
      app.toString

    private def getSortedList[T](
      map: MMap[String, T],
      getSize: T => Int,
    ): List[(String, T)] = map.toList.sortBy { (k, v) => (-getSize(v), k) }
  }

  /** initialize */
  private def init: Unit = for { algo <- spec.algorithms } {
    CaseStat += algo; ElemStat += algo
  }
  init

}
