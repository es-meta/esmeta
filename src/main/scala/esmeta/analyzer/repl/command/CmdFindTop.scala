package esmeta.analyzer.repl.command

trait CmdFindTopDecl { self: Self =>

  // find-top command
  case object CmdFindTop
    extends Command(
      "find-top",
      "Find Top values.",
    ) {
    // options
    val options = Nil

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit =
      Repl.untilTop = true
      Repl.continue = true
  }
}
