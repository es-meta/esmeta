package esmeta.ir

import esmeta.ESMetaTest
import esmeta.LINE_SEP
import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.util.Parsers

/** IR program for tests */
object ProgramParser extends Parsers:
  given Parser[Program] = rep(func) ^^ { Program(_) }
case class Program(funcs: List[Func]) {
  def toCFG: CFG = {
    val builder = new Builder
    for { f <- funcs } builder.translate(f)
    builder.cfg
  }
  override def toString: String =
    funcs.map(_.toString).mkString(LINE_SEP)
}
case object Program extends ProgramParser.From[Program]

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
