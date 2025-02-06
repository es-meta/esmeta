package esmeta.util

import esmeta.util.domain.{*, given}, BSet.*, Flat.*
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

  def walkPair[K, V](
    pair: (K, V),
    kWalk: K => Unit,
    vWalk: V => Unit,
  ): Unit = kWalk(pair._1) -> vWalk(pair._2)

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

  def walkBSet[T](
    set: BSet[T],
    tWalk: T => Unit,
  ): Unit = set match
    case Inf      =>
    case Fin(set) => set.map(tWalk)

  def walkFlat[T](
    flat: Flat[T],
    tWalk: T => Unit,
  ): Unit = flat match
    case Zero      =>
    case One(elem) => tWalk(elem)
    case Many      =>

  def walk(str: String): Unit = {}
  def walk(bool: Boolean): Unit = {}
  def walk(int: Int): Unit = {}
}
