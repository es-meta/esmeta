package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.Expr
import esmeta.util.BaseUtils.*

trait CmdPrintDecl { self: Self =>

// print command
  case object CmdPrint
    extends Command(
      "print",
      "Print specific information",
    ) {
    // options
    val options @ List(reachLoc, ret, expr) =
      List("reach-loc", "return", "expr")

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = {
      val cp = cpOpt.getOrElse(sem.runJobsRp)
      args match {
        case s"-${`reachLoc`}" :: _ =>
          val st = sem.getState(cp)
          st.reachableParts.foreach(println _)
        case s"-${`ret`}" :: _ =>
          val v = cp match
            case np: NodePoint[Node] => println("no return value")
            case rp: ReturnPoint =>
              val ret = sem(rp)
              println(ret.state.getString(ret.value))
        case s"-${`expr`}" :: rest =>
          val str = rest.mkString(" ")
          cp match
            case np: NodePoint[Node] =>
              given NodePoint[Node] = np
              val v = transfer(Expr.from(str))
              println(sem(np).getString(v))

            case rp: ReturnPoint =>
              println("cannot evaluate expression in return point")
        case _ => println("Inappropriate argument")
      }
    }
  }
}
