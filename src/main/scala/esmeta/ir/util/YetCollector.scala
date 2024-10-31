package esmeta.ir.util

import esmeta.ir.*
import scala.collection.mutable.{ListBuffer, Stack}

/** yet expression collector */
object YetCollector:
  def apply(elem: IRElem, ignoreInAssert: Boolean = false): List[EYet] =
    val collector = new YetCollector(ignoreInAssert)
    collector.walk(elem)
    collector.yets.toList
class YetCollector(ignoreInAssert: Boolean) extends UnitWalker:
  var yets: ListBuffer[EYet] = ListBuffer()
  override def walk(inst: Inst): Unit = {
    inst match
      case IAssert(expr) => if (!ignoreInAssert) walk(expr)
      case _             =>
    super.walk(inst)
  }
  override def walk(expr: Expr): Unit = {
    expr match
      case yet: EYet => yets += yet
      case _         =>
    super.walk(expr)
  }
