package esmeta.phase

import esmeta.GlobalConfig
import esmeta.util.ArgParser

/** phase lists
  *
  * @tparam Result
  *   result type of a phase list
  */
sealed trait PhaseList[Result] {
  override def toString: String = names.mkString(" >> ")

  /** append a phase to a phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] =
    PhaseCons(this, phase)

  /** a list of phase names */
  val names: Vector[String]

  /** a list of phases without specific IO types */
  def phases: Vector[Phase[_, _]]

  /** get runner of a phase list */
  def getRunner(parser: ArgParser): GlobalConfig => Result
}

/** empty phase list */
case object PhaseNil extends PhaseList[Unit] {
  val names: Vector[String] = Vector()
  def phases: Vector[Phase[_, _]] = Vector()
  def getRunner(parser: ArgParser): GlobalConfig => Unit = x => {}
}

/** a list construction
  *
  * @tparam P
  *   the result type of the `list` phase list
  * @tparam R
  *   the result type of the `phase` phase
  */
case class PhaseCons[P, R](
  plist: PhaseList[P],
  phase: Phase[P, R],
) extends PhaseList[R] {
  val names: Vector[String] = plist.names :+ phase.name
  def phases: Vector[Phase[_, _]] = plist.phases :+ phase
  def getRunner(
    parser: ArgParser,
  ): GlobalConfig => R =
    val plistRunner = plist.getRunner(parser)
    val phaseRunner = phase.getRunner(parser)
    globalConfig => phaseRunner(plistRunner(globalConfig), globalConfig)
}
