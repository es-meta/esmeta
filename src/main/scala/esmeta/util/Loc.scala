package esmeta.util

/** A trait for objects that have a location in spec.html */
trait Locational {
  var loc: Option[Loc] = None
  def setLoc(start: Pos, end: Pos): this.type = {
    if (loc.isEmpty) loc = Some(Loc("", -1, start, end, List()))
    this
  }
}

/** source locations in algorithms
  *
  * @example
  *   #sec-x[7]:3:2-4:7 (1.2.2) for `Loc("sec-x", 7, Pos(3,2), Pos(4,7),
  *   List(1,2,2,))`
  */
case class Loc(
  var id: String,
  var idx: Int,
  var start: Pos,
  var end: Pos,
  var steps: List[Int],
) {
  // TODO short string for the same line (e.g. 3:2-4)
  override def toString: String =
    s"#$id[$idx]:$start-$end (${steps.mkString(".")})"
}

/** positions in algorithms
  *
  * @example
  *   3:2 for `Pos(3,2)`
  */
case class Pos(
  var line: Int,
  var column: Int,
) {
  override def toString: String = s"$line:$column"
}
