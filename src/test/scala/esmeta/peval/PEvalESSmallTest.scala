package esmeta.peval

import esmeta.*
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*

import esmeta.peval.util.Renamer

/** eval test */
class PEvalESSmallTest extends PEvalTest {
  val name: String = "PEvalESSmallTest"

  // registration
  def init: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    // test for function-declaration-instantiation
    val filename = file.getName
    if (jsFilter(filename)) check(filename) {
      val jsName = file.toString
      val irName = PEvalTest.js2ir(jsName)
      val insts = NormalInsts.fromFile(irName)

      val ast = PEvalTest.scriptParser.fromFile(jsName)

      ()
      // PartialEvaluator(
      //   program = ESMetaTest.program, // : Program,
      //   log = false, // : Boolean = false,
      //   detail = false, // : Boolean = false,
      //   simplifyLevel = 1, // : Int = 1,
      //   logPW = None, // : Option[PrintWriter] = None,
      //   timeLimit = None, // : Option[Int] = None,
      //   renamer = Renamer(), // : Renamer,
      //   // NOTE : renamer should NOT be copied when copying PState - renamer is, specializer-level global state.
      // ).run

      // PEvaluatorTest.checkExit(
      //   PEvaluatorTest.evalFile(ESMetaTest.cfg, jsName, checkAfter = insts),
      // )
    }
  }

  init
}
