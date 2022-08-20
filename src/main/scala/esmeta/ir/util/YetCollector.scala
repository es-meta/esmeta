package esmeta.ir.util

import esmeta.ir.*
import scala.collection.mutable.{ListBuffer, Stack}

/** yet expression collector */
object YetCollector:
  def apply(elem: IRElem): List[EYet] =
    val collector = new YetCollector
    collector.walk(elem)
    collector.yets.toList
class YetCollector extends UnitWalker:
  var yets: ListBuffer[EYet] = ListBuffer()
  override def walk(expr: Expr): Unit =
    expr match
      case yet: EYet => yets += yet
      case _         =>
    super.walk(expr)
