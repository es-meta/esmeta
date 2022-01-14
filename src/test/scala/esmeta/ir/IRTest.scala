package esmeta.ir

import esmeta.ESMetaTest
import esmeta.ir.*
import esmeta.ir.Utils.*

trait IRTest extends ESMetaTest {
  def category: String = "ir"

  // TODO handle evaluation test
  def irEval(st: State): State = Interp(st)
  def irEval(str: String): State =
    Interp(State()).moveTo(Program(str))
  def irEvalFile(filename: String): State = {
    val program = Program.fromFile(filename)
    Interp(State().moveTo(program))
  }

  // tests for IR parser
  def irParseTest(program: Program): Program = {
    val newProgram = Program(program.toString)
    assert(program == newProgram)
    program
  }
  def irParseTest(str: String): Program = irParseTest(Program(str))
  def irParseTestFile(filename: String): Program =
    irParseTest(Program.fromFile(filename))
}
