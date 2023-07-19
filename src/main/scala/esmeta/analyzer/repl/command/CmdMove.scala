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
  def getNps(nid: Int): Array[NodePoint[Node]] = {
    val node = cfg.nodeMap(nid)
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
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = args match {
    case s"-${`to`}" :: nid :: idx :: _ =>
      val nps = getNps(nid.toInt)
      val i = idx.toInt
      if (i < nps.size) {
        val newNp = nps(i)
        REPL.moveCp(newNp)
        println(s"Current control point is moved to $newNp")
      } else {
        println(s"Wrong index: $i")
        printNps(nps)
      }

    case s"-${`to`}" :: nid :: Nil =>
      val nps = getNps(nid.toInt)
      printNps(nps)
    case s"-${`reset`}" :: _ =>
      REPL.restoreCp()
      println("Current control point is restored")
    case _ => println("Inappropriate argument")
  }
}
