package esmeta.spec

import esmeta.LINE_SEP
import esmeta.spec.Utils.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import esmeta.lang.KindCounter
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap}

/** specification statistics */
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

  /** private Stats */
  private val _elemMap: MMap[Element, Stat] = MMap()

  /** counting # of kinds of steps, exprs, cond */
  private val _kindMap: MMap[Algorithm, KindCounter] = MMap()

  /** get total # of kinds of spec */
  def totalKind: (MMap[String, Int], MMap[String, Int], MMap[String, Int]) = {
    val stepMap = MMap[String, Int]()
    val exprMap = MMap[String, Int]()
    val condMap = MMap[String, Int]()

    def add(a: MMap[String, Int], b: MMap[String, Int]): Unit =
      for { (name, count) <- b } {
        a += (name -> (a.getOrElse(name, 0) + count))
      }

    for {
      (algo, counter) <- _kindMap
    } {
      add(stepMap, counter.stepMap)
      add(exprMap, counter.exprMap)
      add(condMap, counter.condMap)
    }

    (stepMap, exprMap, condMap)
  }

  def addAlgo(algo: Algorithm, stat: Stat): Unit = {
    _kindMap += (algo -> algo.stats)

    algo.elem.walkAncestor(
      elem => {
        val elemStat = _elemMap.getOrElse(elem, Stat())
        _elemMap += (elem -> (elemStat + stat))
      },
      (),
      (a, b) => (),
    )
  }

  /** get Yet steps in algorithms of given elem */
  def getYetSteps(elem: Element, indent: Int): String =
    val algos = elem.getAlgos(spec)
    val indentStr = "  " * indent
    algos
      .map(_.incompleteSteps)
      .flatten
      .map(LINE_SEP + indentStr + _.toString(false))
      .fold("")(_ + _)

  // log
  /** get String */
  def getStr(
    elem: Element,
    cKind: String,
    indent: Int = 0,
  ): String =
    val stat = _elemMap.getOrElse(elem, Stat())
    val (pass, total) = stat.get(cKind)
    val fail = total - pass
    val ratioStr = if total != 0 then ratioString(pass, total) else ""

    val yetStepStr =
      if cKind == "Step" & fail != 0 & elem.children.toList.filter(
          _.tagName == "emu-alg",
        ) != Nil
      then getYetSteps(elem, indent + 2)
      else ""

    if elem.tagName == "body" then s"${ratioString(pass, total)}"
    else if elem.tagName == "emu-clause" then
      s"${LINE_SEP}${"  " * indent}- ${elem.id}: ${ratioStr}${yetStepStr}"
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
    if children != Nil then
      children.map(getAllStr(_, cName, indent + 1)).fold(baseStr)(_ + _)
    else baseStr
}
