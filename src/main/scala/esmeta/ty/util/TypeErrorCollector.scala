package esmeta.ty.util

import esmeta.LINE_SEP
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

class TypeErrorCollector extends TyElem {
  import TypeErrorCollector.*, tyStringifier.{*, given}

  val map: MMap[TypeError, MSet[String]] = MMap()

  /** add a type error */
  def add(name: String, error: TypeError): this.type =
    map.getOrElseUpdate(error, MSet.empty).add(name)
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
