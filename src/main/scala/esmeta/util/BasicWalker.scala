package esmeta.util

import scala.collection.mutable.{Map => MMap}

/** the basic walker */
trait BasicWalker {
  def walkOpt[T](
    opt: Option[T],
    tWalk: T => T,
  ): Option[T] = opt.map(tWalk)

  def walkSet[T](
    set: Set[T],
    tWalk: T => T,
  ): Set[T] = set.map(tWalk)

  def walkList[T](
    list: List[T],
    tWalk: T => T,
  ): List[T] = list.map(tWalk)

  def walkVector[T](
    vec: Vector[T],
    tWalk: T => T,
  ): Vector[T] = vec.map(tWalk)

  def walkMap[K, V](
    map: Map[K, V],
    kWalk: K => K,
    vWalk: V => V,
  ): Map[K, V] = map.map { case (k, v) => kWalk(k) -> vWalk(v) }

  def walkMMap[K, V](
    map: MMap[K, V],
    kWalk: K => K,
    vWalk: V => V,
  ): MMap[K, V] = map.map { case (k, v) => kWalk(k) -> vWalk(v) }

  def walk(str: String): String = str
  def walk(bool: Boolean): Boolean = bool
  def walk(int: Int): Int = int
}
