package esmeta.lang.util

import esmeta.lang.*
import scala.collection.mutable.{Map => MMap}

/** a kind collector for metalanguage */
class KindCollector extends UnitWalker {
  import KindCollector.*

  val map: MMap[ClassName, KindData] = MMap()

  private def collect(obj: LangElem): Unit = {
    val name = ClassName(obj.getClass.getSimpleName, Kind(obj))
    val data = map.getOrElseUpdate(name, KindData())
    data.inc
  }

  override def walk(step: Step): Unit =
    collect(step); super.walk(step)
  override def walk(expr: Expression): Unit =
    collect(expr); super.walk(expr)
  override def walk(cond: Condition): Unit =
    collect(cond); super.walk(cond)
}
object KindCollector {
  enum Kind:
    case Step, Expr, Cond, Etc
  object Kind:
    def apply(obj: LangElem): Kind = obj match
      case _: Step       => Kind.Step
      case _: Expression => Kind.Expr
      case _: Condition  => Kind.Cond
      case _             => Kind.Etc

  case class ClassName(name: String, kind: Kind)
  case class KindData(var count: Int = 0) {
    def inc: Unit = count += 1
    def +=(other: KindData): Unit = count += other.count
  }

  def apply(elem: LangElem): KindCollector =
    val counter = new KindCollector
    counter.walk(elem)
    counter
}
