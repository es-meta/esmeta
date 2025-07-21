package esmeta.ty.util

import esmeta.LINE_SEP
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.SystemUtils.*
import scala.collection.concurrent.TrieMap

class TypeErrorCollector extends TyElem {
  import TypeErrorCollector.*, tyStringifier.{*, given}

  val map: TrieMap[TypeError, Set[String]] = TrieMap()

  /** add a type error */
  def add(name: String, error: TypeError): this.type =
    map.updateWith(error) {
      case Some(set) => Some(set + name)
      case None      => Some(Set(name))
    }
    this

  /** add multiple type errors */
  def add(name: String, errors: Iterable[TypeError]): this.type =
    for (error <- errors) this.add(name, error)
    this

  /** dump collected type errors to a file */
  def dumpTo(logDir: String, withNames: Boolean = true) = dumpFile(
    name = "detected type errors",
    data = this.getString(withNames),
    filename = s"$logDir/type-errors",
  )

  def getString(withNames: Boolean): String =
    collectorRule(withNames)(new Appender, this).toString
}
object TypeErrorCollector {
  val tyStringifier = TyElem.getStringifier(false, false)

  /** create a TypeErrorCollector with its name and errors */
  def apply(name: String, errors: Iterable[TypeError]): TypeErrorCollector =
    (new TypeErrorCollector).add(name, errors)
}
