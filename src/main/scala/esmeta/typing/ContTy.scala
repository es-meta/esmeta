package esmeta.typing

import esmeta.cfg.{Func, Node}

/** continuation types */
case class ContTy(map: Map[ContTy.Target, CapturedMapTy]) {

  /** bottom check */
  def isBottom: Boolean = map.isEmpty

  /** union type */
  def |(that: ContTy): ContTy = ContTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) |
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)

  /** intersection type */
  def &(that: ContTy): ContTy = ContTy((for {
    key <- (this.map.keySet & that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) &
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)

  /** prune type */
  def --(that: ContTy): ContTy = ContTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) --
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)
}
object ContTy:
  case class Target(func: Func, returnTo: Node)
  def apply(pairs: (Target, CapturedMapTy)*): ContTy = ContTy(pairs.toMap)
