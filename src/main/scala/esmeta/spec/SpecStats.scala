package esmeta.spec

import esmeta.spec.Utils.*
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
  def addAlgo(docu: Document)(algo: Algorithm, cName: String, int: Int) =
    algo.getElem(docu).foreach(addAncestorCounter(_, cName, int))

  /** Data Structure which binds a counter(Int) to its name */
  private type Counters = MMap[String, Int]
  extension (counters: Counters) {
    def getCounter(cName: String): Int =
      counters.getOrElseUpdate(cName, 0)
  }
}
