package esmeta.ir

import esmeta.ESMetaTest
import esmeta.LINE_SEP
import esmeta.cfgBuilder.*
import esmeta.interpreter.*
import esmeta.ir.util.Parsers
import esmeta.state.*

/** test for IR */
trait IRTest extends ESMetaTest {
  def category: String = "ir"
}
object IRTest {
  // tests for IR parser
  def parseTest(program: Program): Program = {
    val newProgram = Program.from(program.toString)
    assert(program == newProgram)
    program
  }
  def parseFileTest(filename: String): Program =
    parseTest(Program.fromFile(filename))

  // handle interpreter test
  def interp(st: State): State = Interpreter(st)
  def interpFile(filename: String): State =
    interp(State(CFGBuilder(Program.fromFile(filename))))
}
