package esmeta.peval

import esmeta.cfg.CFG
import esmeta.ir.{Name, Temp}

import scala.collection.mutable.{Map as MMap}

class Renamer private(
  names: MMap[(String, Int, Int), String] ,
  temps: MMap[(Int, Int, Int), Int]
) {

  def get(name : Name)(using CFG): Name = ???
  def get(idx : Temp, ctx : PContext)(using CFG): Temp = 
    Temp(temps.getOrElse((idx.idx, ???, ???), newTempCount))

  def newCallCount =
    countCall += 1;
    countCall

  def newTempCount =
    countTemp += 1;
    countTemp

  private var countCall = 0;
  private var countTemp = 0;
  // 변수_함수id_callcount
  // (temp_함수id_callcount) -> map
}

object Renamer {
  def apply() = new Renamer(MMap.empty, MMap.empty);
}