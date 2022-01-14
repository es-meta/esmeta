package esmeta.util

import scala.collection.mutable.{Map => MMap}

/** the basic walker */
trait BasicWalker {
  def walkOpt[T](
    opt: Option[T],
    tWalk: T => T,
  ): Option[T] = opt.map(tWalk)

  def walkList[T](
    list: List[T],
    tWalk: T => T,
  ): List[T] = list.map(tWalk)

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
}
