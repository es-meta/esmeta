package esmeta.es.util.polyfill

import esmeta.ESMetaTest
import esmeta.lang.*

/** test for the polyfill generator */
trait PolyfillTest extends ESMetaTest {
  def category: String = "polyfill"
}
object PolyfillTest {
  import ESMetaTest.*

  lazy val translator = Translator(spec)

  /** the first line of the code translated from a step, inside its scope */
  def step(str: String): String =
    val pb = Builder()
    pb.newScope(translator.compile(pb, Step.from(str)))
      .toString
      .linesIterator
      .map(_.trim)
      .find(line => line.nonEmpty && line != "{")
      .getOrElse("")

  /** the code translated from a condition */
  def cond(str: String): String =
    translator.compile(Builder(), Condition.from(str))

  /** the code translated from an expression */
  def expr(e: Expression): String =
    translator.compile(Builder(), e)

  /** the code checking that `x` has the type */
  def typeCheck(ty: String): String =
    translator.compileTypeCheck(Builder(), "x", ty)
}
