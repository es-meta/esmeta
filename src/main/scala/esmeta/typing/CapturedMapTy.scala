package esmeta.typing

case class CapturedMapTy(map: Map[String, ValueTy] = Map()) {

  /** union type */
  def |(that: CapturedMapTy): CapturedMapTy = CapturedMapTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, BotT) |
    that.map.getOrElse(key, BotT)
  }).toMap)

  /** intersection type */
  def &(that: CapturedMapTy): CapturedMapTy = CapturedMapTy((for {
    key <- (this.map.keySet & that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, BotT) &
    that.map.getOrElse(key, BotT)
  }).toMap)

  /** prune type */
  def --(that: CapturedMapTy): CapturedMapTy = CapturedMapTy((for {
    key <- (this.map.keySet | that.map.keySet).toList
  } yield key -> {
    this.map.getOrElse(key, BotT) --
    that.map.getOrElse(key, BotT)
  }).toMap)
}
