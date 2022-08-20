package esmeta.ir

import esmeta.ESMetaTest

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
}
