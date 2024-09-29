package esmeta.analyzer.repl.command

trait CmdFindImprecDecl { self: Self =>

  // find-imprec command
  case object CmdFindImprec
    extends Command(
      "find-imprec",
      "Find imprecise values.",
    ) {
    // options
    val options = Nil

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit =
      Repl.untilImprec = true
      Repl.continue = true
  }
}
