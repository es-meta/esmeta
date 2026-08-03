package esmeta.fv

import esmeta.cfgBuilder.CFGBuilder
import esmeta.ir.*
import org.scalatest.funsuite.AnyFunSuite

class FVDirectExportTest extends AnyFunSuite {
  private val program = Program.from("@main def main() = { nop }")
  private given esmeta.cfg.CFG = CFGBuilder(program)
  private val template = program.main

  private def emit(body: Inst, main: Boolean = false): String =
    FVDirectExport
      .compileNormalized("direct_test", template.copy(main = main, body = body))
      .source

  test("emit uniform ordinary and continuation wrappers without fallback") {
    val defs = FVDirectExport.compileNormalized(
      "direct_main",
      template.copy(body = IReturn(EMath(1))),
    )

    assert(defs.source.contains("Definition direct_main_inst"))
    assert(
      defs.source.contains(
        "direct_fbody fnames nil true (direct_main_inst mn)",
      ),
    )
    assert(
      defs.source.contains(
        "direct_cont_fbody fnames nil true (direct_main_inst mn)",
      ),
    )
    assert(
      defs
        .ordinaryEntry("all_names")
        .contains("direct_fnsem mn \"main\""),
    )
    assert(defs.continuationEntry("all_names").contains("direct_main_cont"))
    assert(
      defs
        .mainEntry("all_names")
        .exists(_.contains("snd (direct_entry mn")),
    )
    for (fallback <- List("denote_expr", "denote_ref", "denote_inst", "denote_fbody"))
      assert(!defs.source.contains(fallback), fallback)
  }

  test("preserve short-circuit and assignment evaluation order structurally") {
    val right = EList(List(EMath(1)))
    val andSource = emit(IReturn(EBinary(BOp.And, EBool(false), right)))
    val orSource = emit(IReturn(EBinary(BOp.Or, EBool(true), right)))
    assert(andSource.indexOf("VBool false => Ret (VBool false)") < andSource.indexOf("alloc_obj"))
    assert(orSource.indexOf("VBool true => Ret (VBool true)") < orSource.indexOf("alloc_obj"))

    val assignment = emit(IAssign(Field(Name("x"), EStr("p")), EList(Nil)))
    assert(assignment.indexOf("read_target") < assignment.indexOf("alloc_obj"))
    assert(assignment.indexOf("alloc_obj") < assignment.indexOf("write_target"))
  }

  test("shape-specialized comparison and conversion clauses win precedence") {
    def math(e: Expr): Expr = EConvert(COp.ToMath, e)
    val left = ENumber(1.0)
    val right = ENumber(2.0)

    val comparison = emit(IReturn(EBinary(BOp.Lt, math(left), math(right))))
    assert(comparison.contains("denote_number_math_comparison mn BLt"))
    assert(!comparison.contains("direct_binary_value BLt"))

    val shapes = List(
      EConvert(COp.ToNumber, EBinary(BOp.Add, math(left), math(right))) -> "NMAdd BAdd CToNumber",
      EConvert(COp.ToNumber, EBinary(BOp.Mul, math(left), math(right))) -> "NMMul BMul CToNumber",
      EConvert(COp.ToNumber, EBinary(BOp.Div, math(left), math(right))) -> "NMDiv BDiv CToNumber",
      EConvert(COp.ToApproxNumber, EBinary(BOp.Pow, math(left), math(right))) -> "NMPow BPow CToApproxNumber",
      EConvert(COp.ToApproxNumber, EMathOp(MOp.Sin, List(math(left)))) -> "denote_number_sin_value",
    )
    for ((expression, expected) <- shapes) {
      val source = emit(IReturn(expression))
      assert(source.contains(expected), expected)
    }
  }

  test("sequence return, loop, assertions, calls, and SDO remain structural") {
    val sequence = emit(ISeq(List(IReturn(EMath(1)), IPrint(EMath(2)))))
    assert(sequence.contains("CReturn v => Ret"))
    assert(sequence.contains("log_val"))

    val loop = emit(IWhile(EBool(true), IReturn(EMath(1))))
    assert(loop.contains("ITree.iter"))
    assert(loop.contains("Ret (inr"))

    val skipped = emit(IAssert(EYet("marker")))
    assert(!skipped.contains("triggerUB"))
    val checked = emit(IAssert(EBool(false)))
    assert(checked.contains("VBool true"))
    assert(checked.contains("triggerUB"))

    val call = emit(ICall(Name("x"), ERef(Name("f")), List(EMath(1))))
    assert(call.contains("ccallU (ir_sig fn)"))
    assert(call.contains("ccallU cont_invoke_sig"))
    val sdo = emit(ISdoCall(Name("x"), ERef(Name("ast")), "Evaluation", List(EMath(1))))
    assert(sdo.contains("direct_sdo_value fnames"))
    assert(sdo.contains("(fun _ =>"))
  }

  test("parse operands are catchable and unsupported operands fail with context") {
    val source = emit(
      IReturn(EParse(EYet("caught"), EGrammarSymbol("Script", Nil))),
    )
    assert(source.contains("eval_throw"))
    assert(source.contains("alloc_parse_errors"))
    assert(source.contains("direct_parse_outcomes"))

    val error = intercept[FVExport.Unsupported] {
      emit(IReturn(EParse(EDebug(EMath(1)), EGrammarSymbol("Script", Nil))))
    }
    assert(error.getMessage.contains("direct function main"))
    assert(error.getMessage.contains("clause expr.EParse"))
    assert(error.getMessage.contains("unsupported code operand EDebug"))

    val operatorError = intercept[FVExport.Unsupported] {
      emit(IReturn(EBinary(BOp.Xor, EBool(true), EBool(false))))
    }
    assert(operatorError.getMessage.contains("direct function main"))
    assert(operatorError.getMessage.contains("clause export"))
    assert(operatorError.getMessage.contains("bop: ^^"))
  }

  test("ordered coverage matrix is deterministic and clause-specific") {
    val clauses = FVDirectExport.orderedClauseIds
    assert(clauses.distinct == clauses)
    assert(clauses.take(2) == List("ref.RVar", "ref.RField"))
    assert(clauses.indexOf("expr.EBinary.BAnd") < clauses.indexOf("expr.EBinary.general"))
    assert(clauses.indexOf("expr.EConvert.ToNumber.AddMath") < clauses.indexOf("expr.EConvert.general"))
    assert(clauses.indexOf("inst.IAssert.EYet") < clauses.indexOf("inst.IAssert.general"))
    assert(clauses.last == "inst.ISdoCall")
  }
}
