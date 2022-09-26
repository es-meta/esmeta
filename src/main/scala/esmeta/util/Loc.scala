package esmeta.util

import esmeta.LINE_SEP
import scala.annotation.alpha

/** A trait for objects that have a location in spec.html */
trait Locational {
  var loc: Option[Loc] = None
  def setLoc(start: Pos, end: Pos, steps: List[Int]): this.type =
    setLoc(Some(Loc(start, end, steps)))
  def setLoc(locOpt: Option[Loc]): this.type = {
    if (loc.isEmpty) loc = locOpt
    this
  }
}

/** source locations in algorithms
  *
  * @example
  *   3:2-4:7 (1.2.2) for `Loc(Pos(3,2), Pos(4,7), List(1,2,2,))`
  */
case class Loc(
  var start: Pos,
  var end: Pos,
  var steps: List[Int],
) {
  // string getter
  def getString(str: String): String = str.substring(start.offset, end.offset)

  // TODO short string for the same line (e.g. 3:2-4)
  override def toString: String =
    s"(step $stepString, ${start.simpleString}-${end.simpleString})"

  def stepString: String =
    (for ((step, idx) <- steps.zipWithIndex) yield idx % 3 match
      case 0 => step.toString
      case 1 => AlphabetNumeral(step + 1)
      case 2 => RomanNumeral(step, lower = true)
    ).mkString(".")
}

/** positions in algorithms
  *
  * @example
  *   3:2 for `Pos(3,2)`
  */
case class Pos(
  var line: Int,
  var column: Int,
  var offset: Int,
) {
  def simpleString: String = s"$line:$column"
  override def toString: String = s"$simpleString($offset)"
}
