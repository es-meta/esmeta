package esmeta.analyzer.repl.command

import esmeta.analyzer.{Analyzer, repl}

type Self = Decl & repl.Decl & Analyzer

trait Decl
  extends CommandDecl
  with CmdBreakDecl
  with CmdContinueDecl
  with CmdEntryDecl
  with CmdExitDecl
  with CmdFindMergedDecl
  with CmdFindTopDecl
  with CmdGraphDecl
  with CmdHelpDecl
  with CmdInfoDecl
  with CmdJumpDecl
  with CmdListBreakDecl
  with CmdLogDecl
  with CmdMoveDecl
  with CmdPrintDecl
  with CmdRmBreakDecl
  with CmdStopDecl
  with CmdWorklistDecl {
  self: Self =>
}
