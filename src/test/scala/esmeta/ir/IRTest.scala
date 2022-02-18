package esmeta.ir

import esmeta.ESMetaTest
import esmeta.LINE_SEP
import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parsers

/** IR tests */
trait IRTest extends ESMetaTest {
  def category: String = "ir"

  // tests for IR parser
  def irParseTest(program: Program): Program = {
    val newProgram = Program.from(program.toString)
    assert(program == newProgram)
    program
  }
  def irParseTestFile(filename: String): Program =
    irParseTest(Program.fromFile(filename))
}
