package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.util.*
import esmeta.analyzer.repl.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

// graph command
case object CmdGraph
  extends Command(
    "graph",
    "Dump the current control graph.",
  ) {
  import DotPrinter.*

  // options
  val options @ List(total) = List("total")

  // run command
  def apply(
    repl: REPL,
    cpOpt: Option[ControlPoint],
    args: List[String],
  ): Unit = (optional(args.head.toInt), args) match
    case (Some(depth), _) => dumpCFG(repl.sem, cpOpt, depth = Some(depth))
    case (None, s"-$total" :: _) =>
      dumpCFG(repl.sem, cpOpt, depth = None)
    case _ => dumpCFG(repl.sem, cpOpt, depth = Some(0))
}
