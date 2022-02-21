package esmeta.ir

import esmeta.ESMetaTest
import esmeta.LINE_SEP
import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parsers

/** IR tests */
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
}
