package esmeta.spec

import esmeta.LINE_SEP
import esmeta.spec.Utils.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap}

/** specification statistics */
object SpecStats {

  /** private Stats */
  private val _elemMap: MMap[Element, Counters] = MMap()

  /** getter for Stats */
  def elemMap: Map[Element, Counters] = _elemMap.toMap

  /** get counters for given element */
  def getCounters(elem: Element): Counters =
    _elemMap.getOrElseUpdate(elem, MMap())

  /** get 'cName' counter for given element */
  def getCounter(elem: Element, cName: String): Int =
    getCounters(elem).getOrElse(cName, 0)

  /** add given interger to the 'cName' counter (of elem) */
  def addCounter(elem: Element, cName: String, int: Int): Unit =
    val counters = getCounters(elem)
    val pCounter = counters.getCounter(cName)
    counters += cName -> (pCounter + int)
    _elemMap += elem -> counters

  /** add given interger to the 'cName' counters (of elem and elem's ancestors)
    */
  def addAncestorCounter(elem: Element, cName: String, int: Int): Unit =
    addCounter(elem, cName, int)
    val parent = elem.parent
    if (parent != null) addAncestorCounter(parent, cName, int)

  /** add given interger to the 'cName' counters (of elem's ancestors of algo)
    */
  def addAlgo(docu: Document)(algo: Algorithm, cName: String, int: Int): Unit =
    addAncestorCounter(algo.elem, cName, int)

  def addAlgo(
    docu: Document,
  )(algo: Algorithm, targets: List[(String, Int)]): Unit =
    targets.foreach((cName, int) => addAlgo(docu)(algo, cName, int))

  /** get Yet steps in algorithms of given elem */
  def getYetSteps(spec: Spec, elem: Element, indent: Int): String =
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
    spec: Spec,
    elem: Element,
    cKind: String,
    indent: Int = 0,
  ): String =
    val children = elem.children.toList
    val idStr = elem.id
    val indentStr = "  " * indent
    val cNameTotal = s"${cKind}_Total"
    val cNamePass = s"${cKind}_Pass"
    val total = getCounter(elem, cNameTotal)
    val pass = getCounter(elem, cNamePass)
    val fail = total - pass
    val ratioStr = if total != 0 then " " + ratioString(pass, total) else ""
    val yetStepStr =
      if cKind == "Step" & fail != 0 & children.filter(
          _.tagName == "emu-alg",
        ) != Nil
      then getYetSteps(spec, elem, indent + 2)
      else ""
    if elem.tagName == "body" then s"${ratioString(pass, total)}"
    else if elem.tagName == "emu-clause" then
      s"${LINE_SEP}${indentStr}- ${idStr} :${ratioStr}${yetStepStr}"
    else ""

  /** get descendant String */
  def getAllStr(
    spec: Spec,
    elem: Element,
    cName: String,
    indent: Int = 0,
  ): String =
    val baseStr = getStr(spec, elem, cName, indent)
    val children = elem.children.toList
    if children != Nil then
      children.map(getAllStr(spec, _, cName, indent + 1)).fold(baseStr)(_ + _)
    else baseStr

  /** Data Structure which binds a counter(Int) to its name */
  private type Counters = MMap[String, Int]
  extension (counters: Counters) {
    def getCounter(cName: String): Int =
      counters.getOrElseUpdate(cName, 0)
  }
}
