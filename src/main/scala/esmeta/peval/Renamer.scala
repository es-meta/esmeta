package esmeta.peval

import esmeta.cfg.CFG
import esmeta.ir.{Name, Temp, Var, Global, Local}
import esmeta.state.{DynamicAddr}
import scala.collection.mutable.{Map as MMap}

class Renamer private (
  // names: MMap[(String, Int, Int), String] ,
  temps: MMap[(Int, Int, Int), Int],
  private var countCall: Int = 0,
  private var countTemp: Int = 0,
  private var dynamicAddr: Long = 0,
) {

  def get(x: Var, ctx: PContext)(using CFG): Var = x match
    case Global(name) => Global(name)
    case l: Local     => get(l, ctx)

  def get(l: Local, ctx: PContext)(using CFG): Local = l match
    case name: Name => get(name, ctx)
    case idx: Temp  => get(idx, ctx)

  def get(name: Name, ctx: PContext)(using cfg: CFG): Name =
    Name(s"${name.name}_${cfg.fnameMap(ctx.func.name).id}_${ctx.sensitivity}")

  def get(idx: Temp, ctx: PContext)(using cfg: CFG): Temp =
    val key = (idx.idx, cfg.fnameMap(ctx.func.name).id, ctx.sensitivity);
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
