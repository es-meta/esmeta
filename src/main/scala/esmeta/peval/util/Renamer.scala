package esmeta.peval.util

import esmeta.ir.{Name, Temp, Var, Global, Local}
import esmeta.state.{DynamicAddr}
import scala.collection.mutable.{Map as MMap}
import esmeta.peval.pstate.*
import esmeta.util.BaseUtils.cached

class Renamer private (
  // names: MMap[(String, Int, Int), String] ,
  temps: MMap[(Int, Int, Int), Int],
  private var countCall: Int = 0,
  private var countTemp: Int = 0,
  private var dynamicAddr: Long = 0,
) {

  /* to distinguish func names when renaming locals, WITHOUT USING CFG. */
  val funcNameToInt =
    var count = 0;
    cached { _ =>
      count += 1; count
    }

  def get(x: Var, ctx: PContext): Var = x match
    case Global(name) => Global(name)
    case l: Local     => get(l, ctx)

  def get(l: Local, ctx: PContext): Local = l match
    case name: Name => get(name, ctx)
    case idx: Temp  => get(idx, ctx)

  def get(name: Name, ctx: PContext): Name =
    Name(s"${name.name}_${funcNameToInt(ctx.func.name)}_${ctx.sensitivity}")

  def get(idx: Temp, ctx: PContext): Temp =
    val key = (idx.idx, funcNameToInt(ctx.func.name), ctx.sensitivity);
    temps.get(key) match
      case None =>
        val i = newTempCount;
        temps += key -> i; Temp(i)
      case Some(v) => Temp(v)

  def newCallCount =
    countCall += 1;
    countCall

  def newTempCount =
    countTemp += 1;
    countTemp

  def newAddr =
    dynamicAddr += 1;
    DynamicAddr(dynamicAddr)

  // 변수_함수id_callcount
  // (temp_함수id_callcount) -> map
}

object Renamer {
  def apply() = new Renamer(MMap.empty);
}
