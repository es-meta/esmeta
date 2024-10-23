package esmeta.peval

import esmeta.*
import esmeta.ir.NormalInsts
import esmeta.peval.util.{AstHelper}
import esmeta.util.SystemUtils.*
import scala.util.{Try, Success, Failure}
import esmeta.cfgBuilder.CFGBuilder

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
      val target = ESMetaTest.program.funcs
        .find(_.name == FUNC_DECL_INSTANT)
        .getOrElse(fail(s"target ${FUNC_DECL_INSTANT} not found in Program"))
      val decls = AstHelper.getFuncDecls(ast)

      if !decls.isEmpty then {

        val overloads = decls.zipWithIndex.flatMap((fd, idx) =>

          val (renamer, pst) =
            PartialEvaluator.ForECMAScript.prepareForFDI(target, fd);

          val peval = PartialEvaluator(
            program = ESMetaTest.program,
            renamer = renamer,
          )

          val pevalResult = Try(
            peval.run(
              target,
              pst,
              Some(s"${target.name}${idx}"),
            ),
          ).map(_._1)

          pevalResult match
            case Success(newFunc)   => Some((newFunc, fd))
            case Failure(exception) => fail(s"peval failed for FDI"), // None
        )

        val sfMap = PartialEvaluator.ForECMAScript.genMap(overloads)
        val newCFG = CFGBuilder
          .byIncremental(ESMetaTest.cfg, overloads.map(_._1), sfMap)
          .getOrElse(fail("Cfg incremental build fail"))

        PEvalTest.checkExit(
          PEvalTest.evalFile(newCFG, jsName, checkAfter = insts),
        )
      }
    }
  }

  init
}
