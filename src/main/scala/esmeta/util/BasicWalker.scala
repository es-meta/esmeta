package esmeta.util

import esmeta.util.domain.{*, given}, BSet.*, Flat.*
import scala.collection.mutable.{Map => MMap}

/** the basic walker */
trait BasicWalker {

  def walkOpt[T](
    opt: Option[T],
    tWalk: T => T,
  ): Option[T] = opt.map(tWalk)

  def walkIterable[T](
    set: Iterable[T],
    tWalk: T => T,
  ): Iterable[T] = set.map(tWalk)

  def walkSet[T](
    set: Set[T],
    tWalk: T => T,
  ): Set[T] = set.map(tWalk)

  def walkList[T](
    list: List[T],
    tWalk: T => T,
  ): List[T] = list.map(tWalk)

  def walkPair[K, V](
    pair: (K, V),
    kWalk: K => K,
    vWalk: V => V,
  ): (K, V) = (kWalk(pair._1), vWalk(pair._2))

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

  def walkBSet[T](
    set: BSet[T],
    tWalk: T => T,
  ): BSet[T] = set match
    case Inf      => Inf
    case Fin(set) => Fin(set.map(tWalk))

  def walkFlat[T](
    flat: Flat[T],
    tWalk: T => T,
  ): Flat[T] = flat match
    case Zero      => Zero
    case One(elem) => One(tWalk(elem))
    case Many      => Many

  def walk(str: String): String = str
  def walk(bool: Boolean): Boolean = bool
  def walk(int: Int): Int = int
}
