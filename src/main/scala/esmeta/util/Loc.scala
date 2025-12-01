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
    originText: Option[String] = None,
    steps: List[Int] = Nil,
  ): this.type = setLoc(Some(Loc(start, end, filename, originText, steps)))

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
  start: Pos,
  end: Pos,
  var filename: Option[String] = None,
  var originText: Option[String] = None,
  var steps: List[Int] = Nil,
) {

  /** get the string from the original string */
  def getString(str: String): String = str.substring(start.offset, end.offset)

  /** get range string */
  def rangeString: String = s"$start-$end"

  /** get the full line at the start position from the original string */
  def getLine(str: String): String =
    if (str.isEmpty) return ""
    val off =
      if (start.offset < 0) 0
      else if (start.offset >= str.length) str.length - 1
      else start.offset
    val lineStart = str.lastIndexOf('\n', off) match
      case -1 => 0
      case i  => i + 1
    val lineEnd = str.indexOf('\n', off) match
      case -1 => str.length
      case j  => j
    str.substring(lineStart, lineEnd).replace("\r", "")

  /** get step string */
  def stepString: String =
    (for ((step, idx) <- steps.zipWithIndex) yield idx % 3 match
      case 0 => step.toString
      case 1 => AlphabetNumeral(step)
      case 2 => RomanNumeral(step, lower = true)
    ).mkString(".")

  /** find the full spec line for this location using step information */
  def findStepFullLine(code: String): Option[String] =
    val prefix = s"${stepString}. "
    // scan for last prefix occurrence at or before start.offset
    var idx = -1
    var from = 0
    var found = code.indexOf(prefix, from)
    while found >= 0 && found <= start.offset do
      idx = found
      from = found + 1
      found = code.indexOf(prefix, from)
    // if none found before, fallback to first occurrence
    if (idx == -1) idx = code.indexOf(prefix)
    if (idx == -1) Some(Loc.oneLine(replaceStepPrefix(getLine(code))))
    else
      val lineStart = code.lastIndexOf('\n', idx) match
        case -1 => 0
        case i  => i + 1
      val lineEnd = code.indexOf('\n', idx) match
        case -1 => code.length
        case j  => j
      val raw = code.substring(lineStart, lineEnd)
      Some(Loc.oneLine(replaceStepPrefix(raw)))

  /** Replace a fixed numeric step at the beginning of a line with loc's step */
  def replaceStepPrefix(line: String): String =
    val major = stepString.takeWhile(_.isDigit) match
      case s if s.nonEmpty => s
      case _               => stepString
    val StepNum = "^(\\s*)(\\d+)(\\.\\s+)(.*)$".r
    line match
      case StepNum(ws, _, dotSpace, rest) => s"$ws$major$dotSpace$rest"
      case _                              => line

  /** merge locations */
  def merge(that: Loc): Option[Loc] =
    val Loc(start, _, lname, ltext, lsteps) = this
    val Loc(_, end, rname, rtext, rsteps) = that
    if (lname != rname || ltext != rtext || lsteps != rsteps) return None
    Some(Loc(start, end, lname, ltext, lsteps))

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
object Loc {

  /** sanitize a string to a single line */
  def oneLine(s: String): String =
    s.replace("\r", " ").replace("\n", " ").trim

  /** drop the leading step prefix like "1. " from a line, if present */
  def dropStepPrefix(s: String): String =
    s.indexOf(". ") match
      case -1 => s
      case i  => s.substring(i + 2)
}

/** positions in algorithms
  *   - `line` starts from 1
  *   - `column` starts from 1
  *   - `offset` starts from 0
  *
  * @example
  *   3:2(5) for `Pos(3,2,5)` -- line 3, column 2, offset 5
  */
case class Pos(
  line: Int,
  column: Int,
  offset: Int,
) {

  final def +(that: Pos): Pos =
    Pos(
      this.line + that.line - 1,
      if (that.line == 1) this.column + that.column - 1 else that.column,
      0,
      this.offset + that.offset,
    )

  /** conversion to string */
  override def toString: String = s"$line:$column($offset)"
}

/** ordering of locations */
given Ordering[Loc] = Ordering.by(loc => (loc.start, loc.end))

/** ordering of positions */
given Ordering[Pos] = Ordering.by(pos => (pos.line, pos.column))
