package esmeta.lang.util

import esmeta.lang.*
import scala.collection.mutable.{Map => MMap}
import scala.annotation.targetName

/** a kind counter for metalanguage */
class KindCounter extends UnitWalker {
  type Counter = MMap[String, Int]

  val stepMap: Counter = MMap()
  val exprMap: Counter = MMap()
  val condMap: Counter = MMap()

  private def add(obj: LangElem, map: Counter): Unit =
    val name = obj.getClass.getSimpleName
    map += name -> (map.getOrElse(name, 0) + 1)

  override def walk(step: Step): Unit =
    add(step, stepMap)
    super.walk(step)
  override def walk(expr: Expression): Unit =
    add(expr, exprMap)
    super.walk(expr)
  override def walk(cond: Condition): Unit =
    add(cond, condMap)
    super.walk(cond)
}
object KindCounter {
  def apply(elem: LangElem): KindCounter =
    val counter = new KindCounter
    counter.walk(elem)
    counter
}
