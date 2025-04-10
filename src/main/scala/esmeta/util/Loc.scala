package esmeta.util

import esmeta.util.Appender.*

/** A trait for objects that have a source location */
trait Locational {

  /** source location */
  var loc: Option[Loc] = None

  /** set source location with start and end positions and steps */
  def setLoc(
    start: Pos,
    end: Pos,
    filename: Option[String] = None,
    steps: List[Int] = Nil,
  ): this.type = setLoc(Some(Loc(start, end, filename, steps)))

  /** set source location if not already set */
  def setLoc(locOpt: Option[Loc]): this.type =
    if (loc.isEmpty) loc = locOpt
    this

  /** merge source locations */
  def mergeLoc(that: Locational): Option[Loc] = for {
    lloc <- this.loc
    rloc <- that.loc
    loc <- lloc merge rloc
  } yield loc
}

/** source locations in algorithms
  *
  * @example
  *   (step 1.ii.c, 3:2-4:7) for `Loc(Pos(3,2), Pos(4,7), None, List(1,2,3))`
  *   (3:2-4:7 @ path) for `Loc(Pos(3,2), Pos(4,7), Some("path"), Nil)`
  */
case class Loc(
  var start: Pos,
  var end: Pos,
  var filename: Option[String] = None,
  var steps: List[Int] = Nil,
) {

  /** get the string from the original string */
  def getString(str: String): String = str.substring(start.offset, end.offset)

  /** get range string */
  def rangeString: String = s"$start-$end"

  /** get step string */
  def stepString: String =
    (for ((step, idx) <- steps.zipWithIndex) yield idx % 3 match
      case 0 => step.toString
      case 1 => AlphabetNumeral(step)
      case 2 => RomanNumeral(step, lower = true)
    ).mkString(".")

  /** merge locations */
  def merge(that: Loc): Option[Loc] =
    val Loc(start, _, lname, lsteps) = this
    val Loc(_, end, rname, rsteps) = that
    if (lname != rname || lsteps != rsteps) return None
    Some(Loc(start, end, lname, lsteps))

  /** conversion to string */
  override def toString: String =
    val app = new Appender
    app >> "("
    if (steps.nonEmpty) app >> "step " >> stepString >> ", "
    app >> rangeString
    filename.map(app >> " @ " >> _)
    app >> ")"
    app.toString
}

/** positions in algorithms
  *
  * @example
  *   3:2(5) for `Pos(3,2,5)` -- line 3, column 2, offset 5
  */
case class Pos(
  var line: Int,
  var column: Int,
  var offset: Int,
) {

  /** conversion to string */
  override def toString: String = s"$line:$column($offset)"
}

/** ordering of locations */
given Ordering[Loc] = Ordering.by(loc => (loc.start, loc.end))

/** ordering of positions */
given Ordering[Pos] = Ordering.by(pos => (pos.line, pos.column))
