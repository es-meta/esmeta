package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.Expr
import esmeta.util.BaseUtils.*

// print command
case object CmdPrint
  extends Command(
    "print",
    "Print specific information",
  ) {
  // options
  val options @ List(reachLoc, ret, expr) = List("reach-loc", "return", "expr")

  // run command
  def apply(
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = {
    val cp = cpOpt.getOrElse(sem.runJobsRp)
    args match {
      case s"-${`reachLoc`}" :: _ => {
        val st = sem.getState(cp)
        st.reachableParts.foreach(println _)
      }
      case s"-${`ret`}" :: _ => {
        val v = cp match {
          case np: NodePoint[Node] => println("no return value")
          case rp: ReturnPoint =>
            val ret = sem(rp)
            println(ret.state.getString(ret.value))
        }
      }
      case s"-${`expr`}" :: rest => {
        val str = rest.mkString(" ")
        val v = analyzer.transfer(cp, Expr.from(str))
        val st = cp match {
          case np: NodePoint[Node] => sem(np)
          case rp: ReturnPoint     => sem(rp).state
        }
        println(st.getString(v))
      }
      case _ => println("Inappropriate argument")
    }
  }
}
