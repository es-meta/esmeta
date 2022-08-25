package esmeta.ai.repl.command

import esmeta.ai.*
import esmeta.ai.repl.*
import esmeta.util.BaseUtils.*

// info command
case object CmdInfo
  extends Command(
    "info",
    "Show abstract state of node",
  ) {
  // options
  val options @ List(ret, block, detail, callsite) =
    List("ret", "block", "detail", "callsite")

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = args match {
    case opt :: optArgs if options contains opt.substring(1) =>
      showInfo(repl, opt.substring(1), optArgs)
    case _ =>
      cpOpt match {
        case Some(cp) =>
          val detail = args.headOption == Some("-detail")
          println(repl.cpInfo(cp, detail))
          println
        case None =>
          showInfo(repl, ret, List("RunJobs"))
      }
  }

  // show information
  def showInfo(repl: REPL, opt: String, optArgs: List[String]): Unit = {
    val sem = repl.sem
    val info = (opt, optArgs) match {
      case (`ret`, target :: _) =>
        val fname = target
        sem.rpMap.keySet.filter(_.func.name == fname)
      case (`block`, target :: _) if optional(target.toInt) != None =>
        val uid = target.toInt
        sem.npMap.keySet.filter(_.node.id == uid)
      case (`callsite`, _) =>
        val cp = repl.curCp.get
        val rp = ReturnPoint(cp.func, cp.view)
        sem.retEdges(rp)
      case _ =>
        println("Inappropriate argument")
        Set()
    }
    info.foreach(cp => { println(repl.cpInfo(cp, true)); println })
  }
}
