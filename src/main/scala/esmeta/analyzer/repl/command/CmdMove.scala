package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.domain
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import scala.Console.*

// Move command
case object CmdMove
  extends Command(
    "move",
    "Move to specified control point.",
  ) {
  // options
  val options @ List(to, reset) = List("to", "reset")

  // get node points of given nid
  def getNps(repl: REPL, nid: Int): Array[NodePoint[Node]] = {
    val sem = repl.sem
    val node = Config.cfg.nodeMap(nid)
    sem.npMap.keys.toArray.filter(_.node.id == node.id)
  }

  // print node points
  def printNps(nps: Array[NodePoint[Node]]) = {
    nps.zipWithIndex.foreach {
      case (np, idx) =>
        val npStr = np.toString(detail = true)
        println(s"  $idx: $npStr")
    }
  }

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = args match {
    case s"-${`to`}" :: nid :: idx :: _ =>
      val nps = getNps(repl, nid.toInt)
      val i = idx.toInt
      if (i < nps.size) {
        val newNp = nps(i)
        repl.moveCp(newNp)
        println(s"Current control point is moved to $newNp")
      } else {
        println(s"Wrong index: $i")
        printNps(nps)
      }

    case s"-${`to`}" :: nid :: Nil =>
      val nps = getNps(repl, nid.toInt)
      printNps(nps)
    case s"-${`reset`}" :: _ =>
      repl.restoreCp()
      println("Current control point is restored")
    case _ => println("Inappropriate argument")
  }
}
