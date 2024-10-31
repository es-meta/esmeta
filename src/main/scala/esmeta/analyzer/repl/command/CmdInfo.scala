package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*

trait CmdInfoDecl { self: Self =>

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
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = args match {
      case opt :: optArgs if options contains opt.substring(1) =>
        showInfo(opt.substring(1), optArgs)
      case _ =>
        cpOpt match {
          case Some(cp) =>
            val detail = args.headOption == Some("-detail")
            println(Repl.cpInfo(cp, detail))
            println
          case None =>
            showInfo(ret, List("RunJobs"))
        }
    }

    // show information
    def showInfo(opt: String, optArgs: List[String]): Unit = {
      val info = (opt, optArgs) match {
        case (`ret`, target :: _) =>
          val fname = target
          rpMap.keySet.filter(_.func.name == fname)
        case (`block`, target :: _) if optional(target.toInt) != None =>
          val uid = target.toInt
          npMap.keySet.filter(_.node.id == uid)
        case (`callsite`, _) =>
          val cp = Repl.curCp.get
          val rp = ReturnPoint(cp.func, cp.view)
          retEdges(rp)
        case _ =>
          println("Inappropriate argument")
          Set()
      }
      info.foreach(cp => { println(Repl.cpInfo(cp, true)); println })
    }
  }
}
