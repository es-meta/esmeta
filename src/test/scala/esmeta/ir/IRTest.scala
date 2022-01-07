package esmeta.ir

import esmeta.ESMetaTest
import esmeta.ir._

trait IRTest extends ESMetaTest {
  def category: String = "ir"

  // TODO eval IR codes
  // def irEval(st: State): State = Interp(st)
  // def irEval(str: String): State =
  //   Interp(State(InstCursor).moveTo(Program(str)))
  // def irEvalFile(filename: String): State = {
  //   val program = Program.fromFile(filename)
  //   Interp(State(InstCursor, fnameOpt = Some(filename)).moveTo(program))
  // }

  // tests for IR parser
  // def irParseTest(program: Program): Program = {
  //   // val newProgram = Program(program.toString)
  //   val newProgram = ???
  //   assert(program == newProgram)
  //   program
  // }
  // def irParseTest(str: String): Program = ??? // irParseTest(Program(str))
  // def irParseTestFile(filename: String): Program = ???
}
