package esmeta.typing

import akka.parboiled2.RuleTrace.Capture

/** closure types */
case class CloTy(map: Map[String, CapturedMapTy]) {

  /** bottom check */
  def isBottom: Boolean = map.isEmpty

  /** union type */
  def |(that: CloTy): CloTy = CloTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) |
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)

  /** intersection type */
  def &(that: CloTy): CloTy = CloTy((for {
    key <- (this.map.keySet & that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) &
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)

  /** prune type */
  def --(that: CloTy): CloTy = CloTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, CapturedMapTy()) --
    that.map.getOrElse(key, CapturedMapTy())
  }).toMap)
}
object CloTy:
  def apply(pairs: (String, CapturedMapTy)*): CloTy = CloTy(pairs.toMap)
