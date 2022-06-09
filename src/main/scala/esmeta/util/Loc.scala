package esmeta.util

/** A trait for objects that have a location in spec.html */
trait Locational {
  var loc: Option[Loc] = None
  def setLoc(start: Pos, end: Pos, steps: List[Int]): this.type = {
    if (loc.isEmpty) loc = Some(Loc(start, end, steps))
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
  // TODO short string for the same line (e.g. 3:2-4)
  override def toString: String =
    s"$start-$end (${steps.mkString(".")})"
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
  override def toString: String = s"$line:$column($offset)"
}
