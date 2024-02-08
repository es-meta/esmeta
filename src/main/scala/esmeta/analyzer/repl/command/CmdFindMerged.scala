package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import esmeta.util.BaseUtils.*

trait CmdFindMergedDecl { self: Self =>

// find-merged command
  case object CmdFindMerged
    extends Command(
      "find-merged",
      "Find merged analysis results.",
    ) {
    // options
    val options = Nil

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = cpOpt.map(cp => {
      val st = cp match {
        case np: NodePoint[Node] => sem(np)
        case rp: ReturnPoint     => sem(rp).state
      }
      st.findMerged
    })
  }
}
