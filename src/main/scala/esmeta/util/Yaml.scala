package esmeta.util

import esmeta.util.Appender.{*, given}

/** YAML object */
sealed trait Yaml:
  override def toString: String = (new Appender >> this).toString
case class YMap(items: Iterable[(String, Yaml)]) extends Yaml
case class YList(items: Iterable[Yaml]) extends Yaml
case class YString(value: String) extends Yaml
object Yaml:
  def apply(any: Any): Yaml = any match
    case map: Map[_, _] =>
      YMap(map.toList.map { case (k, v) => k.toString -> apply(v) })
    case seq: Seq[_] => YList(seq.map(apply))
    case k: Int      => YString(f"$k%,d")
    case _           => YString(any.toString)
  def apply(seq: (String, Any)*): Yaml =
    YMap(seq.map { case (k, v) => k -> apply(v) })
