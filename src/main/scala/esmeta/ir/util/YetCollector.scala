package esmeta.ir.util

import esmeta.ir.*
import scala.collection.mutable.{ListBuffer, Stack}

/** yet expression collector */
object YetCollector:
  def apply(elem: IRElem): List[(EYet, Option[Func])] =
    val collector = new YetCollector
    collector.walk(elem)
    collector.yets.toList
class YetCollector extends UnitWalker:
  var yets: ListBuffer[(EYet, Option[Func])] = ListBuffer()
  var funcs: Stack[Func] = Stack()
  override def walk(func: Func): Unit =
    funcs.push(func)
    super.walk(func)
    funcs.pop
  override def walk(expr: Expr): Unit =
    expr match
      case yet: EYet => yets += ((yet, funcs.headOption))
      case _         =>
    super.walk(expr)
