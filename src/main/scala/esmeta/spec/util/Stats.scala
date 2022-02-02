package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.spec.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import esmeta.lang.util.KindCounter
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap}

/** specification statistics to each element */
case class Stat(
  algoPass: Int = 0,
  algoTotal: Int = 0,
  stepPass: Int = 0,
  stepTotal: Int = 0,
) {
  def +(other: Stat): Stat = {
    val Stat(ap, at, sp, st) = other
    Stat(ap + algoPass, at + algoTotal, sp + stepPass, st + stepTotal)
  }

  def get(ckind: String): (Int, Int) = ckind match
    case "Algo" => (algoPass, algoTotal)
    case "Step" => (stepPass, stepTotal)
    case _      => ???
}

/** specification statistics */
class Stats(spec: Spec) {

  /** maps from element to stat */
  private val _elemMap: MMap[Element, Stat] = MMap()

  /** counting # of kinds of steps, exprs, cond */
  private val _kindMap: MMap[Algorithm, KindCounter] = MMap()

  /** initialize */
  private def init: Unit = {
    // iter algorithms in spec
    for { algo <- spec.algorithms } {
      // counter kinds in algorithm
      _kindMap += algo -> KindCounter(algo.body)

      // statisitics for algo
      val algoStat = Stat(
        if algo.complete then 1 else 0,
        1,
        algo.completeSteps.length,
        algo.steps.length,
      )

      // walk ancestors
      algo.elem.walkAncestor(
        elem =>
          _elemMap += (elem -> (_elemMap.getOrElse(elem, Stat()) + algoStat)),
        (),
        (a, b) => (),
      )
    }
  }
  init

  /** get total # of kinds of spec */
  def totalKind: (MMap[String, Int], MMap[String, Int], MMap[String, Int]) = {
    val stepMap = MMap[String, Int]()
    val exprMap = MMap[String, Int]()
    val condMap = MMap[String, Int]()

    def add(a: MMap[String, Int], b: MMap[String, Int]): Unit =
      for { (name, count) <- b } {
        a += (name -> (a.getOrElse(name, 0) + count))
      }

    for { (algo, counter) <- _kindMap } {
      add(stepMap, counter.stepMap)
      add(exprMap, counter.exprMap)
      add(condMap, counter.condMap)
    }

    (stepMap, exprMap, condMap)
  }

  /** get String */
  def getStr(
    elem: Element,
    cKind: String,
    indent: Int = 0,
  ): String =

    // get new line string
    def newline(indent: Int = 0): String = LINE_SEP + "  " * indent

    // get ratio string
    val stat = _elemMap.getOrElse(elem, Stat())
    val (pass, total) = stat.get(cKind)
    val fail = total - pass
    val ratioStr = if total != 0 then s" ${ratioString(pass, total)}" else ""

    // get yet steps
    val yetStepStr =
      if cKind == "Step" & fail != 0 & elem.children.toList.filter(
          _.tagName == "emu-alg",
        ) != Nil
      then
        // get algos in same emu-clause
        val algos = spec.algorithms.filter(_.elem.getId == elem.id)

        // get yet steps
        algos
          .map(_.incompleteSteps)
          .flatten
          .map(newline(indent + 2) + _.toString(false))
          .fold("")(_ + _)
      else ""

    // final result
    if elem.tagName == "body" then s"${ratioString(pass, total)}"
    else if elem.tagName == "emu-clause" then
      s"${newline(indent)}- ${elem.id}:${ratioStr}${yetStepStr}"
    else ""

  /** get descendant String */
  def getAllStr(cName: String): String = getAllStr(spec.document.body, cName)
  def getAllStr(
    elem: Element,
    cName: String,
    indent: Int = 0,
  ): String =
    val baseStr = getStr(elem, cName, indent)
    val children = elem.children.toList
    if !children.isEmpty then
      children.map(getAllStr(_, cName, indent + 1)).fold(baseStr)(_ + _)
    else baseStr
}
