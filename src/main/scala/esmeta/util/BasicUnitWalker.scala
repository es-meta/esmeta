package esmeta.util

import scala.collection.mutable.{Map => MMap}

/** the basic unit walker */
trait BasicUnitWalker {
  def walkOpt[T](
    opt: Option[T],
    tWalk: T => Unit,
  ): Unit = opt.foreach(tWalk)

  def walkIterable[T](
    set: Iterable[T],
    tWalk: T => Unit,
  ): Unit = set.foreach(tWalk)

  def walkSet[T](
    set: Set[T],
    tWalk: T => Unit,
  ): Unit = set.foreach(tWalk)

  def walkList[T](
    list: List[T],
    tWalk: T => Unit,
  ): Unit = list.foreach(tWalk)

  def walkVector[T](
    vec: Vector[T],
    tWalk: T => Unit,
  ): Unit = vec.foreach(tWalk)

  def walkMap[K, V](
    map: Map[K, V],
    kWalk: K => Unit,
    vWalk: V => Unit,
  ): Unit = map.foreach { case (k, v) => kWalk(k); vWalk(v) }

  def walkMMap[K, V](
    map: MMap[K, V],
    kWalk: K => Unit,
    vWalk: V => Unit,
  ): Unit = map.foreach { case (k, v) => kWalk(k) -> vWalk(v) }

  def walk(str: String): Unit = {}
  def walk(bool: Boolean): Unit = {}
  def walk(int: Int): Unit = {}
}
