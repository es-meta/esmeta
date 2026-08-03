package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.error.{
  InvalidBinaryOp,
  InvalidConversion,
  InvalidExit,
  InvalidMathOp,
}
import esmeta.es.Lexical
import esmeta.extractor.Extractor
import esmeta.ir.*
import esmeta.state.{
  Bool,
  Heap,
  Infinity,
  Math,
  Number,
  RecordObj,
  State,
  Str,
  Undef,
  Value,
}
import esmeta.ty.*
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.Locale
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.{Map => MMap}

class FVTyModelTest extends AnyFunSuite {
  private def withTempTyModel(data: String)(body: String => Unit): Unit = {
    val path = Files.createTempFile("fv-ty-model-", ".json")
    try {
      Files.writeString(path, data)
      body(path.toString)
    } finally Files.deleteIfExists(path)
  }

  test("load TyModel declarations strictly and without partial success") {
    withTempTyModel(
      """{"TyModel":{"decls":["type Parent {}","type Child extends Parent {}"]}}""",
    ) { path =>
      assert(FVTyModel.readDecls(path).map(_.name) == List("Parent", "Child"))
    }

    val invalidJson = intercept[IllegalArgumentException] {
      withTempTyModel("{")(FVTyModel.readDecls)
    }
    assert(invalidJson.getMessage.contains("invalid TyModel JSON"))

    for (data <- List("{}", """{"TyModel":{"decls":"not-an-array"}}""")) {
      val invalidStructure = intercept[IllegalArgumentException] {
        withTempTyModel(data)(FVTyModel.readDecls)
      }
      assert(invalidStructure.getMessage.contains("TyModel.decls structure"))
    }

    val empty = intercept[IllegalArgumentException] {
      withTempTyModel("""{"TyModel":{"decls":[]}}""")(FVTyModel.readDecls)
    }
    assert(empty.getMessage.contains("TyModel.decls is empty"))

    val invalidDecl = intercept[IllegalArgumentException] {
      withTempTyModel(
        """{"TyModel":{"decls":["type Valid {}","not a type declaration"]}}""",
      )(FVTyModel.readDecls)
    }
    assert(invalidDecl.getMessage.contains("TyModel.decls[1]"))
  }

  test("reject a missing TyModel input instead of treating it as empty") {
    val path = Files.createTempFile("fv-ty-model-missing-", ".json")
    Files.delete(path)
    val error = intercept[IllegalArgumentException] {
      FVTyModel.readDecls(path.toString)
    }
    assert(error.getMessage.contains("cannot read TyModel input"))
  }

  test("derive the _CoqProject TyModel shard block from generated counts") {
    val project =
      """Fragment.v
        |# BEGIN GENERATED FVTyModel SHARDS
        |stale.v
        |# END GENERATED FVTyModel SHARDS
        |Domain.v
        |""".stripMargin
    val rendered = FVTyModel.renderCoqProject(project, 2, 1)

    assert(
      rendered ==
      """Fragment.v
          |# BEGIN GENERATED FVTyModel SHARDS
          |TyModelBase.v
          |TyModelParent00.v
          |TyModelParent01.v
          |TyModelBindings00.v
          |TyModel.v
          |# END GENERATED FVTyModel SHARDS
          |Domain.v
          |""".stripMargin,
    )
  }

  test("Rocq float literals are complete and locale-independent") {
    classOf[Locale].synchronized {
      val original = Locale.getDefault
      try {
        Locale.setDefault(Locale.GERMANY)
        assert(FVExport.floatLit(Double.NaN) == "PrimFloat.nan")
        assert(
          FVExport.floatLit(Double.PositiveInfinity) ==
          "PrimFloat.infinity",
        )
        assert(
          FVExport.floatLit(Double.NegativeInfinity) ==
          "PrimFloat.neg_infinity",
        )
        assert(FVExport.floatLit(0.0) == "(0.0000000000000000)%float")
        assert(FVExport.floatLit(-0.0) == "(-0.0000000000000000)%float")
        assert(FVExport.floatLit(1.25) == "(1.2500000000000000)%float")
        assert(FVExport.floatLit(-0.5) == "(-0.50000000000000000)%float")
        assert(!FVExport.floatLit(-0.5).contains("- -"))
        assert(!FVExport.floatLit(1.25).contains(","))
      } finally Locale.setDefault(original)
    }
  }

  test("capture intentional bare EYet separately from unsafe assert skips") {
    val program = Program.from(
      """@main def main() = {
        |  nop
        |}""".stripMargin,
    )
    val interp =
      new FVExport.CapturingInterpreter(State(CFGBuilder(program)))

    interp.eval(IAssert(EYet("intentionally uncompiled assertion")))
    interp.eval(IAssert(ERef(Name("missing"))))

    assert(interp.skippedYetAsserts == 1)
    assert(interp.failedToEvaluateAsserts == 1)

    assert(FVInitState.skippedAssertBlocker(3, 0).isEmpty)
    val blocker = FVInitState.skippedAssertBlocker(2, 4).get
    assert(blocker.contains("4 non-EYet assertion(s)"))
    assert(blocker.contains("2 bare EYet assertion(s)"))

    def exportProgram(
      body: String,
    ): Either[FVExport.Skipped, FVExport.Exported] = {
      val path = Files.createTempFile("fv-assert-", ".ir")
      try {
        Files.writeString(path, s"@main def main() = { $body }")
        FVExport.exportFile(path.toString)
      } finally Files.deleteIfExists(path)
    }

    assert(exportProgram("""assert (yet "intentional")""").isRight)
    val unsafe = exportProgram("assert missing")
    assert(unsafe.isLeft)
    assert(unsafe.swap.toOption.get.reason.contains("1 non-EYet assertion(s)"))
  }

  test("reject a Test262 assertion failure as an invalid oracle exit") {
    FVInitState.requireSuccessfulTest262Exit(Undef)
    val error = intercept[InvalidExit] {
      FVInitState.requireSuccessfulTest262Exit(Bool(false))
    }
    assert(error.value == Bool(false))
  }

  test("parse persistent Test262 exporter requests strictly") {
    assert(
      FVInitState.parseExporterRequest("EXPORT 12 34") ==
      Right(Some((12, 34))),
    )
    assert(FVInitState.parseExporterRequest("QUIT") == Right(None))
    assert(FVInitState.parseExporterRequest("EXPORT -1 2").isLeft)
    assert(FVInitState.parseExporterRequest("EXPORT 1 nope").isLeft)
    assert(FVInitState.parseExporterRequest("EXPORT 1 2 trailing").isLeft)
  }

  test("parse Test262 export concurrency strictly") {
    assert(FVInitState.parseTest262ExportJobs(Nil) == 1)
    assert(
      FVInitState.parseTest262ExportJobs(
        List("--payload-only", "--test262-export-jobs=12"),
      ) == 12,
    )
    intercept[IllegalArgumentException] {
      FVInitState.parseTest262ExportJobs(List("--test262-export-jobs=0"))
    }
    intercept[IllegalArgumentException] {
      FVInitState.parseTest262ExportJobs(List("--test262-export-jobs=nope"))
    }
    intercept[IllegalArgumentException] {
      FVInitState.parseTest262ExportJobs(
        List("--test262-export-jobs=2", "--test262-export-jobs=3"),
      )
    }
  }

  test("parallel Test262 preparation preserves input order") {
    val allStarted = CountDownLatch(3)
    val result = FVInitState.mapTest262WithJobs(List(3, 1, 2), jobs = 3) {
      value =>
        allStarted.countDown()
        assert(allStarted.await(5, TimeUnit.SECONDS))
        value * 10
    }

    assert(result == List(30, 10, 20))
  }

  test("parallel payload tasks keep independent AST origin namespaces") {
    val roots = List.fill(4)(Lexical("IdentifierName", "x"))
    val origins = FVInitState.mapTest262WithJobs(roots, jobs = 4) { root =>
      val other = Lexical("IdentifierName", "x")
      val allocator = FVInitState.AstOriginAllocator(Some(root))
      (allocator.id(root), allocator.id(other), allocator.id(root))
    }

    assert(origins == List.fill(4)((0, 1, 0)))
  }

  test("allocate AST origins independently per generated program") {
    val firstCached = Lexical("IdentifierName", "x")
    val structurallyEqual = Lexical("IdentifierName", "x")
    val first = FVInitState.AstOriginAllocator(Some(firstCached))

    assert(first.id(firstCached) == 0)
    assert(first.id(firstCached) == 0)
    assert(first.id(structurallyEqual) == 1)

    val second = FVInitState.AstOriginAllocator(Some(structurallyEqual))
    assert(second.id(structurallyEqual) == 0)
    assert(second.id(firstCached) == 1)
  }

  test("capture Math-to-Number only beyond the exact integral boundary") {
    val exactLimit = BigDecimal(scala.math.BigInt(1) << 53)

    assert(!FVInitState.needsMathToNumberCapture(exactLimit))
    assert(!FVInitState.needsMathToNumberCapture(-exactLimit))
    assert(FVInitState.needsMathToNumberCapture(exactLimit + 1))
    assert(FVInitState.needsMathToNumberCapture(-exactLimit - 1))
    assert(
      !FVInitState.needsMathToNumberCapture(exactLimit + BigDecimal("0.5")),
    )
  }

  test("ToNumber and ToApproxNumber share Math host-capture boundaries") {
    val program = Program.from("@main def main() = { nop }")
    val exactLimit = scala.math.BigInt(1) << 53

    for (cop <- List(COp.ToNumber, COp.ToApproxNumber)) {
      def evaluate(input: scala.math.BigInt): (Number, Int) = {
        val interp =
          new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
        val result =
          interp
            .eval(EConvert(cop, EMath(BigDecimal(input))))
            .asInstanceOf[Number]
        (result, interp.capturedHostEntryCount)
      }

      assert(evaluate(exactLimit)._2 == 0)
      assert(evaluate(-exactLimit)._2 == 0)
      val (positiveResult, positiveCaptures) = evaluate(exactLimit + 1)
      assert(positiveResult == Number((exactLimit + 1).toDouble))
      assert(positiveCaptures == 1)
      assert(evaluate(-exactLimit - 1)._2 == 1)
    }
  }

  test("host capture mirrors extended-Math conversion behavior") {
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))

    for (cop <- List(COp.ToNumber, COp.ToApproxNumber)) {
      assert(
        interp.eval(EConvert(cop, EMath(1))) == Number(1.0),
      )
      assert(
        interp.eval(EConvert(cop, ENumber(Double.PositiveInfinity))) ==
        Number(Double.PositiveInfinity),
      )
      assert(
        interp.eval(EConvert(cop, ENumber(Double.NegativeInfinity))) ==
        Number(Double.NegativeInfinity),
      )
      assert(
        interp.eval(EConvert(cop, EInfinity(true))) ==
        Number(Double.PositiveInfinity),
      )
      assert(
        interp.eval(EConvert(cop, EInfinity(false))) ==
        Number(Double.NegativeInfinity),
      )
    }
  }

  test("MathOp capture preserves finite and infinite extended-Math results") {
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))

    assert(interp.eval(EMathOp(MOp.Exp, List(EMath(0)))) == Math(1))
    assert(interp.capturedHostEntryCount == 1)

    assert(
      interp.eval(EMathOp(MOp.Exp, List(EMath(1000)))) == Infinity(true),
    )
    assert(interp.capturedHostEntryCount == 2)
    assert(
      interp.capturedHostEntries.collect {
        case FVInitState.HostCapture.MathOp(MOp.Exp, _, result) => result
      } == List(Math(1), Infinity(true)),
    )

    assertThrows[FVExport.Unsupported] {
      interp.eval(EMathOp(MOp.Sqrt, List(EMath(2))))
    }
    assertThrows[InvalidMathOp] {
      interp.eval(EMathOp(MOp.Sqrt, List(EMath(-1))))
    }
  }

  test("classify model-boundary failures separately from ESMeta failures") {
    assert(
      FVInitState.classifyTest262Failure(
        FVExport.Unsupported("fractional Math host value"),
      ) == FVInitState.Test262FailureClass.NotRepresentable(
        "fractional Math host value",
      ),
    )
    assert(
      FVInitState.classifyTest262Failure(
        new IllegalStateException("interpreter defect"),
      ) == FVInitState.Test262FailureClass.ESMetaFailed,
    )
  }

  test("Test262 control options do not become the generated Spec source") {
    assert(
      FVInitState.sourceForArgs(
        Array("--test262-shard", "0", "1280", "built-ins/Array"),
      ) == "var x = 1;",
    )
    assert(
      FVInitState.sourceForArgs(
        Array("--test262-server", "built-ins/Array"),
      ) == "var x = 1;",
    )
    assert(
      FVInitState.sourceForArgs(Array("--js-equiv")) == "var x = 1;",
    )
    assert(FVInitState.sourceForArgs(Array("let x = 2;")) == "let x = 2;")
  }

  test("fractional lexical Math is omitted without rounding other SDOs") {
    val number = Number(1.1)
    val entries = List[(String, Value)](
      "MV" -> Math(BigDecimal("1.1")),
      "NumericValue" -> number,
      "SV" -> Str("kept"),
      "TV" -> Undef,
      "integer" -> Math(7),
    )
    assert(
      FVInitState.representableLexicalSdos(entries) ==
      List(
        "NumericValue" -> number,
        "SV" -> Str("kept"),
        "TV" -> Undef,
        "integer" -> Math(7),
      ),
    )
  }

  test("extended-Math payload keeps Math and Infinity value tags") {
    def payload(value: esmeta.state.ExtMath): Array[Byte] =
      FVPayload.encode(0) { out =>
        FVInitState.writeExtMathValuePayload(value, out)
      }

    val valueOffset = FVPayload.Magic.length + Integer.BYTES
    assert(payload(Math(7))(valueOffset) == 0.toByte)
    assert(
      payload(Infinity(true))
        .drop(valueOffset)
        .sameElements(Array[Byte](12, 1)),
    )
    assert(
      payload(Infinity(false))
        .drop(valueOffset)
        .sameElements(Array[Byte](12, 0)),
    )
    assertThrows[FVExport.Unsupported] {
      payload(Math(BigDecimal("0.5")))
    }
  }

  test("emit exact MathTy record-field constraints") {
    assert(FVTyModel.mathConstraint(MathTy.Top) == "RFCMath")
    assert(
      FVTyModel.mathConstraint(MathTy.NonNeg) ==
      "(RFCMathSign false true true)",
    )
    assert(
      FVTyModel.mathConstraint(MathTy.NonNegInt) ==
      "(RFCMathIntSign false true true)",
    )
    assert(
      FVTyModel.fieldConstraint(NonNegIntT) ==
      "(RFCMathIntSign false true true)",
    )
    assert(
      FVTyModel.mathConstraint(MathIntTy(Set(-2, 0, 3).map(BigInt(_)))) ==
      "(RFCMathIntSet ((-2) :: 0 :: 3 :: nil))",
    )
    assert(
      FVTyModel.mathConstraint(
        MathSetTy(Set(Math("-1.5"), Math(0), Math(2), Math("3.25"))),
      ) == "(RFCMathSet (0 :: 2 :: nil))",
    )
  }

  test("export Math integer type tests and ETrim without approximation") {
    val program = Program.from("@main def main() = { nop }")
    given esmeta.cfg.CFG = CFGBuilder(program)

    assert(FVExport.rocqTy(Type(IntT)) == "(TMathInt true true true)")
    assert(
      FVExport.rocqTy(Type(ConstructorT)) ==
      "(TRecordFields \"Object\" (\"Call\" :: \"Construct\" :: nil))",
    )
    assert(
      FVExport.rocqTy(Type(FunctionT)) ==
      "(TRecordFields \"Object\" (\"Call\" :: nil))",
    )
    val heap = Heap()
    val descendant = RecordObj("BuiltinFunctionObject", MMap())
    val exactObject = RecordObj(
      "Object",
      MMap("Call" -> Undef, "Construct" -> Undef),
    )
    val missingConstruct = RecordObj("Object", MMap("Call" -> Undef))
    val unrelated = RecordObj(
      "ModuleRecord",
      MMap("Call" -> Undef, "Construct" -> Undef),
    )
    assert(FunctionT.record.contains(descendant, heap))
    assert(ConstructorT.record.contains(descendant, heap))
    assert(ConstructorT.record.contains(exactObject, heap))
    assert(!ConstructorT.record.contains(missingConstruct, heap))
    assert(!ConstructorT.record.contains(unrelated, heap))
    assert(
      FVExport.rocqTy(Type(EnumT("unset"))) ==
      "(TEnumNames (\"unset\" :: nil))",
    )
    assert(
      FVExport.rocqTypeAnnotation(Type(CompT)) ==
      "(mkTypeAnnotation \"Completion\" (Some TCompletion))",
    )
    val typedCompletion = Func(
      main = false,
      kind = FuncKind.AbsOp,
      name = "Completion",
      params = List(
        Param(Name("completionRecord"), Type(CompT), optional = false),
      ),
      retTy = Type(CompT),
      body = IReturn(ERef(Name("completionRecord"))),
    )
    assert(
      FVExport
        .rocqFunc(typedCompletion)
        .startsWith(
          "mkTypedFunc false FKAbsOp \"Completion\" " +
          "(\"completionRecord\" :: nil) " +
          "((mkParamAnnotation " +
          "(mkTypeAnnotation \"Completion\" (Some TCompletion)) false) " +
          ":: nil) (mkTypeAnnotation \"Completion\" " +
          "(Some TCompletion))",
        ),
    )
    assert(
      FVExport.rocqTy(Type(RecordT("", List("Value", "Key")))) ==
      "(TRecordFields \"\" (\"Key\" :: \"Value\" :: nil))",
    )
    assert(
      FVExport.rocqTy(Type(NonNegIntT)) ==
      "(TMathInt false true true)",
    )
    assert(
      FVExport.rocqTy(Type(ListT(NonNegIntT))) ==
      "(TListOf (TMathInt false true true))",
    )
    assert(
      FVExport.rocqTy(Type(IntT(Set(BigInt(3), BigInt(-2), BigInt(0))))) ==
      "(TMathIntSet ((-2) :: 0 :: 3 :: nil))",
    )
    assert(
      FVExport.rocqTy(Type(NegInfinityT)) ==
      "(TInfinity true false)",
    )
    assert(
      FVExport.rocqTy(Type(PosInfinityT)) ==
      "(TInfinity false true)",
    )
    assert(FVExport.rocqTy(Type(FalseT)) == "(TBoolSet true false)")
    assert(FVExport.rocqTy(Type(TrueT)) == "(TBoolSet false true)")
    assert(
      FVExport.rocqTy(Type(StrT("reject", "handle", "한😀"))) ==
      "(TStrSet ((cu \"handle\") :: (cu \"reject\") :: " +
      "(54620 :: 55357 :: 56832 :: nil) :: nil))",
    )
    assert(
      FVExport.rocqTy(
        Type(
          IntT(Set(BigInt(1))) || NegInfinityT || StrT("x") || FalseT,
        ),
      ) ==
      "(TUnion ((TMathIntSet (1 :: nil)) :: (TInfinity true false) :: " +
      "(TStrSet ((cu \"x\") :: nil)) :: (TBoolSet true false) :: nil))",
    )

    val input = "\uFEFF x\u2029"
    assert(
      FVExport.rocqExpr(ETrim(EStr(input), true)) ==
      s"(ETrim (EStr ${FVExport.cstrLit(input)}) true)",
    )
    assert(
      FVExport.rocqExpr(ETrim(EStr(input), false)) ==
      s"(ETrim (EStr ${FVExport.cstrLit(input)}) false)",
    )
    assert(
      FVExport.rocqExpr(
        EMathOp(MOp.Atan2, List(EMath(1), EMath(-1))),
      ) == "(EMathOp MAtan2 ((EMath 1) :: (EMath (-1)) :: nil))",
    )
    assert(
      MOp.values.toList.map(FVExport.rocqMOp) == List(
        "MExpm1",
        "MLog10",
        "MLog2",
        "MCos",
        "MCbrt",
        "MExp",
        "MCosh",
        "MSinh",
        "MTanh",
        "MAcos",
        "MAcosh",
        "MAsinh",
        "MAtanh",
        "MAsin",
        "MAtan2",
        "MAtan",
        "MLog1p",
        "MLog",
        "MSin",
        "MSqrt",
        "MTan",
      ),
    )
    assert(
      MOp.values.toList.map(FVInitState.mathOpTag) == (0 to 20).toList,
    )
    assert(
      new String(FVPayload.Magic, java.nio.charset.StandardCharsets.US_ASCII) ==
      "ESFVIT07",
    )
    assert(FVInitState.mathToNumberHostTag == 7)
    assert(
      FVInitState.NumberMathOp.values.toList.map(FVInitState.numberMathOpTag) ==
      List(0, 1, 2, 3),
    )
    assert(
      FVInitState.NumberMathOp.values.toList
        .map(FVInitState.rocqNumberMathOp) ==
      List("NMAdd", "NMMul", "NMDiv", "NMPow"),
    )
    assert(FVInitState.numberMathHostTag == 8)
    assert(FVInitState.numberSinHostTag == 9)
    assert(FVInitState.numberMathCompareHostTag == 10)
    assert(FVInitState.numberToMathHostTag == 11)
  }

  test("EParse exporter admits only the catchable operand fragment") {
    val program = Program.from("@main def main() = { nop }")
    given esmeta.cfg.CFG = CFGBuilder(program)

    val supported = EParse(
      ERef(Field(Name("this"), EMath(0))),
      EGrammarSymbol("Script", Nil),
    )
    assert(FVExport.rocqExpr(supported).startsWith("(EParse "))
    assert(FVExport.parseOperandSupported(EYet("caught")))
    assert(FVExport.parseOperandSupported(ESourceText(EList(Nil))))

    val unsupported = EParse(
      EBinary(BOp.Add, EMath(1), EMath(2)),
      EGrammarSymbol("Script", Nil),
    )
    val error = intercept[FVExport.Unsupported] {
      FVExport.rocqExpr(unsupported)
    }
    assert(error.getMessage.contains("EParse unsupported code operand"))
  }

  test("capture current ESMeta Number Math composites exactly") {
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))

    def composite(
      cop: COp,
      bop: BOp,
      left: Double,
      right: Double,
    ): Number =
      interp
        .eval(
          EConvert(
            cop,
            EBinary(
              bop,
              EConvert(COp.ToMath, ENumber(left)),
              EConvert(COp.ToMath, ENumber(right)),
            ),
          ),
        )
        .asInstanceOf[Number]

    assert(composite(COp.ToNumber, BOp.Add, 0.1, 0.2) == Number(0.3))
    assert(composite(COp.ToNumber, BOp.Add, 1.5, 2.25) == Number(3.75))
    val product = composite(COp.ToNumber, BOp.Mul, 0.0, -1.0)
    assert(product == Number(0.0))
    assert(!esmeta.util.BaseUtils.isNegZero(product.double))
    assert(composite(COp.ToNumber, BOp.Div, 0.3, 0.1) == Number(3.0))
    assert(
      composite(COp.ToApproxNumber, BOp.Pow, 0.1, 2.0) == Number(0.01),
    )
    assert(interp.capturedHostEntryCount == 5)

    assert(
      composite(COp.ToApproxNumber, BOp.Pow, 1.0e308, 1.5) ==
      Number(Double.PositiveInfinity),
    )
    assert(
      composite(COp.ToApproxNumber, BOp.Pow, -1.0e308, 3.0) ==
      Number(Double.NegativeInfinity),
    )
    assert(interp.capturedHostEntryCount == 7)
    assertThrows[InvalidBinaryOp] {
      composite(COp.ToApproxNumber, BOp.Pow, -1.0, 0.5)
    }

    val uncached = interp.eval(
      EConvert(
        COp.ToNumber,
        EBinary(
          BOp.Add,
          EConvert(COp.ToMath, EMath(1)),
          EConvert(COp.ToMath, EMath(2)),
        ),
      ),
    )
    assert(uncached == Number(3.0))
    assert(interp.capturedHostEntryCount == 7)
  }

  test("Number Math composites fail on the left before evaluating the right") {
    val program = Program.from("@main def main() = { nop }")

    for (left <- List(Double.NaN, Double.PositiveInfinity)) {
      val interp =
        new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
      val error = intercept[InvalidConversion] {
        interp.eval(
          EConvert(
            COp.ToNumber,
            EBinary(
              BOp.Add,
              EConvert(COp.ToMath, ENumber(left)),
              EConvert(
                COp.ToMath,
                EMathOp(MOp.Exp, List(EMath(1000))),
              ),
            ),
          ),
        )
      }
      assert(error.v == Number(left))
      assert(interp.capturedHostEntryCount == 0)
    }

    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    val error = intercept[InvalidConversion] {
      interp.eval(
        EConvert(
          COp.ToNumber,
          EBinary(
            BOp.Add,
            EConvert(COp.ToMath, ENumber(Double.NaN)),
            EConvert(
              COp.ToMath,
              EMathOp(MOp.Sqrt, List(EMath(-1))),
            ),
          ),
        ),
      )
    }
    assert(error.v == Number(Double.NaN))
  }

  test("Number Math host query keys distinguish op and signed zero") {
    import FVInitState.{HostCapture, NumberMathOp}
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    def query(
      op: NumberMathOp,
      left: Double,
      right: Double,
    ): HostCapture =
      HostCapture.NumberMathOp(op, left, right, Number(0.0))

    assert(
      interp.sameQuery(
        query(NumberMathOp.Add, 0.0, 1.0),
        query(NumberMathOp.Add, 0.0, 1.0),
      ),
    )
    assert(
      !interp.sameQuery(
        query(NumberMathOp.Add, 0.0, 1.0),
        query(NumberMathOp.Mul, 0.0, 1.0),
      ),
    )
    assert(
      !interp.sameQuery(
        query(NumberMathOp.Add, 0.0, 1.0),
        query(NumberMathOp.Add, -0.0, 1.0),
      ),
    )
    assert(
      interp.sameQuery(
        query(NumberMathOp.Add, Double.NaN, 1.0),
        query(NumberMathOp.Add, Double.NaN, 1.0),
      ),
    )
  }

  test("ToMath comparison composites preserve decimal comparison semantics") {
    import FVInitState.{
      HostCapture,
      NumberMathCompareDirection,
      NumberMathCompareOp,
    }
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    def compare(op: BOp, left: Expr, right: Expr): Value =
      interp.eval(
        EBinary(
          op,
          EConvert(COp.ToMath, left),
          EConvert(COp.ToMath, right),
        ),
      )

    assert(compare(BOp.Lt, ENumber(-0.0), ENumber(0.0)) == Bool(false))
    assert(compare(BOp.Lt, ENumber(-1.5), EMath(-1)) == Bool(true))
    assert(compare(BOp.Lt, EMath(-2), ENumber(-1.5)) == Bool(true))
    assert(compare(BOp.Equal, ENumber(2.0), EBigInt(2)) == Bool(true))
    assert(compare(BOp.Equal, ENumber(2.5), EBigInt(2)) == Bool(false))
    assert(compare(BOp.Lt, ENumber(-3.0), EMath(-3)) == Bool(false))
    val decimalInteger = scala.math.BigInt("123456789012345670000")
    assert(
      compare(
        BOp.Equal,
        ENumber(1.2345678901234567e20),
        EBigInt(decimalInteger),
      ) == Bool(true),
    )
    assert(
      interp.capturedHostEntries.contains(
        HostCapture.NumberMathCompare(
          NumberMathCompareOp.Equal,
          NumberMathCompareDirection.NumberLeft,
          1.2345678901234567e20,
          decimalInteger,
          Bool(true),
        ),
      ),
    )

    def query(
      op: NumberMathCompareOp,
      direction: NumberMathCompareDirection,
      number: Double,
    ): HostCapture = HostCapture.NumberMathCompare(
      op,
      direction,
      number,
      0,
      Bool(false),
    )
    val base = query(
      NumberMathCompareOp.Lt,
      NumberMathCompareDirection.NumberLeft,
      0.0,
    )
    assert(interp.sameQuery(base, base))
    assert(
      !interp.sameQuery(
        base,
        query(
          NumberMathCompareOp.Equal,
          NumberMathCompareDirection.NumberLeft,
          0.0,
        ),
      ),
    )
    assert(
      !interp.sameQuery(
        base,
        query(
          NumberMathCompareOp.Lt,
          NumberMathCompareDirection.NumberRight,
          0.0,
        ),
      ),
    )
    assert(
      !interp.sameQuery(
        base,
        query(
          NumberMathCompareOp.Lt,
          NumberMathCompareDirection.NumberLeft,
          -0.0,
        ),
      ),
    )
  }

  test(
    "capture Scala BigDecimal Number-to-Math results without approximation",
  ) {
    import FVInitState.HostCapture
    val program = Program.from("@main def main() = { nop }")
    val interp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    val input = 1.2345678901234567e20
    val decimal = BigDecimal(input)
    assert(decimal.toBigInt == scala.math.BigInt("123456789012345670000"))
    assert(
      interp.eval(EConvert(COp.ToMath, ENumber(input))) == Math(decimal),
    )
    assert(
      interp.capturedHostEntries ==
      List(HostCapture.NumberToMath(input, Math(decimal))),
    )

    val mixed =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    assert(
      mixed.eval(
        EConvert(
          COp.ToNumber,
          EBinary(
            BOp.Add,
            EConvert(COp.ToMath, ENumber(2.0)),
            EConvert(COp.ToMath, EMath(3)),
          ),
        ),
      ) == Number(5.0),
    )
    assert(
      mixed.capturedHostEntries ==
      List(HostCapture.NumberToMath(2.0, Math(2))),
    )
  }

  test(
    "generated composite inventory stays at twelve Lt and two Equal shapes",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val rendered = cfg.program.funcs
      .flatMap { func =>
        try Some(FVExport.rocqFunc(func))
        catch case _: FVExport.Unsupported => None
      }
      .mkString("\n")
    def count(needle: String): Int =
      rendered.sliding(needle.length).count(_ == needle)

    // TimeClip uses pure Number comparisons so it does not require host-cache
    // entries that are absent from the unnormalized ESMeta oracle run.
    assert(count("(EBinary BLt (EConvert CToMath") == 10)
    assert(count("(EBinary BEqual (EConvert CToMath") == 2)
    assert(
      count(
        "(EConvert CToApproxNumber (EMathOp MSin " +
        "((EConvert CToMath",
      ) == 1,
    )
  }

  test("capture only the finite-Number MSin terminal composite") {
    import FVInitState.HostCapture
    val program = Program.from("@main def main() = { nop }")
    def sinExpr(inner: Expr): Expr =
      EConvert(
        COp.ToApproxNumber,
        EMathOp(MOp.Sin, List(EConvert(COp.ToMath, inner))),
      )

    val numberInterp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    val result = numberInterp.eval(sinExpr(ENumber(0.5)))
    assert(result == Number(math.sin(0.5)))
    assert(
      numberInterp.capturedHostEntries ==
      List(HostCapture.NumberSin(0.5, Number(math.sin(0.5)))),
    )
    assert(
      numberInterp.sameQuery(
        HostCapture.NumberSin(0.0, Number(0.0)),
        HostCapture.NumberSin(0.0, Number(1.0)),
      ),
    )
    assert(
      !numberInterp.sameQuery(
        HostCapture.NumberSin(0.0, Number(0.0)),
        HostCapture.NumberSin(-0.0, Number(0.0)),
      ),
    )

    val mathInterp =
      new FVInitState.HostCapturingInterpreter(State(CFGBuilder(program)))
    assert(mathInterp.eval(sinExpr(EMath(0))) == Number(0.0))
    assert(
      mathInterp.capturedHostEntries ==
      List(HostCapture.MathOp(MOp.Sin, List(BigDecimal(0)), Math(0))),
    )
  }

  test("validate Number Math terminal shapes without rewriting") {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val cases = List(
      "Number::add" -> FVInitState.NumberMathOp.Add,
      "Number::multiply" -> FVInitState.NumberMathOp.Mul,
      "Number::divide" -> FVInitState.NumberMathOp.Div,
      "Number::exponentiate" -> FVInitState.NumberMathOp.Pow,
    )
    for ((name, op) <- cases) {
      val target = cfg.program.funcs.filter(_.name == name).head
      assert(FVExport.normalizeForRocq(target) eq target)
      val body = target.body.asInstanceOf[ISeq]
      val missing = target.copy(body = ISeq(body.insts.dropRight(1)))
      val missingError = intercept[FVExport.Unsupported] {
        FVExport.normalizeForRocq(missing)
      }
      assert(missingError.msg.contains("terminal shape drift"))
      val wrong = target.copy(
        body =
          ISeq(body.insts.updated(body.insts.length - 1, IReturn(EMath(0)))),
      )
      val wrongError = intercept[FVExport.Unsupported] {
        FVExport.normalizeForRocq(wrong)
      }
      assert(wrongError.msg.contains("terminal shape drift"))
      assert(
        FVExport.countExprOccurrences(
          target.body,
          FVExport.numberMathTerminalExpr(op),
        ) == 1,
      )

      val metadataDrifts = List(
        target.copy(main = true),
        target.copy(kind = FuncKind.AbsOp),
        target.copy(name = s"$name.drift"),
        target.copy(params = target.params.dropRight(1)),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(lhs = Name("drift")),
          ),
        ),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(optional = true),
          ),
        ),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(ty = Type(MathT)),
          ),
        ),
        target.copy(retTy = Type(MathT)),
      )
      for (drift <- metadataDrifts) {
        val error = intercept[FVExport.Unsupported] {
          FVExport.validateNumberMathTerminal(drift, op)
        }
        assert(error.msg.contains("terminal shape drift"))
      }
    }
  }

  test(
    "normalize selected finite Number truncation pairs once and fail closed",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val selected =
      List("ToIntegerOrInfinity", "ToInt32", "ToUint32", "ToUint16")

    for (name <- selected) {
      val target = cfg.program.funcs.filter(_.name == name).head
      val original = target.body.asInstanceOf[ISeq].insts
      val normalized = FVExport.normalizeForRocq(target)
      val changed = normalized.body.asInstanceOf[ISeq].insts
      val differing =
        original.indices.filter(idx => original(idx) != changed(idx))
      assert(differing.size == 1)
      val idx = differing.head
      val tmp = original(idx) match
        case IAssign(local: Local, _) => local
        case other => fail(s"expected assignment, got $other")
      assert(original(idx) == FVExport.truncOriginalAssign(tmp))
      assert(changed(idx) == FVExport.truncReplacementAssign(tmp))
      assert(original(idx + 1) == FVExport.truncTowardZero(tmp))
      assert(changed.updated(idx, original(idx)) == original)

      val metadataDrifts = List(
        target.copy(main = true),
        target.copy(kind = FuncKind.NumMeth),
        target.copy(params = Nil),
        target.copy(
          params = target.params.map(_.copy(lhs = Name("drift"))),
        ),
        target.copy(
          params = target.params.map(_.copy(optional = true)),
        ),
        target.copy(
          params = target.params.map(_.copy(ty = Type(MathT))),
        ),
        target.copy(retTy = Type(MathT)),
      )
      for (drift <- metadataDrifts) {
        val error = intercept[FVExport.Unsupported] {
          FVExport.normalizeFiniteNumberTruncation(drift)
        }
        assert(error.msg.contains("finite Number truncation shape drift"))
      }

      val missingGuard = target.copy(
        body = ISeq(original.updated(idx - 1, INop())),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(missingGuard)
      }
      val mutatedProducer = target.copy(
        body = ISeq(original.updated(0, INop())),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(mutatedProducer)
      }
      val removedProducer = target.copy(
        body = ISeq(original.drop(1)),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(removedProducer)
      }
      val separated = target.copy(
        body = ISeq(original.patch(idx + 1, List(INop()), 0)),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(separated)
      }
      val preNormalized = target.copy(body =
        ISeq(original.updated(idx, FVExport.truncReplacementAssign(tmp))),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(preNormalized)
      }
      val nestedOriginal = IIf(
        EBool(true),
        ISeq(
          List(
            FVExport.truncOriginalAssign(Temp(99)),
            FVExport.truncTowardZero(Temp(99)),
          ),
        ),
        ISeq(Nil),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(
          target.copy(body = ISeq(original :+ nestedOriginal)),
        )
      }
      val nestedReplacement = IIf(
        EBool(true),
        ISeq(
          List(
            FVExport.truncReplacementAssign(Temp(99)),
            FVExport.truncTowardZero(Temp(99)),
          ),
        ),
        ISeq(Nil),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeFiniteNumberTruncation(
          target.copy(body = ISeq(original :+ nestedReplacement)),
        )
      }
    }

    for (name <- List("ToInt16", "ToInt8", "ToUint8")) {
      val target = cfg.program.funcs.filter(_.name == name).head
      assert(FVExport.normalizeForRocq(target) eq target)
    }
  }

  test("normalize only proven integral floor divisions and fail closed") {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val selected = List(
      "INTRINSICS.Array.prototype.reverse",
      "INTRINSICS.TypedArray.prototype.reverse",
      "TypedArrayLength",
    )

    for (name <- selected) {
      val target = cfg.program.funcs.filter(_.name == name).head
      val originalInsts = target.body.asInstanceOf[ISeq].insts
      val normalized = FVExport.normalizeForRocq(target)
      val normalizedInsts = normalized.body.asInstanceOf[ISeq].insts
      val differing =
        originalInsts.indices.filter(idx =>
          originalInsts(idx) != normalizedInsts(idx),
        )
      assert(differing.size == 1)
      val idx = differing.head
      assert(normalizedInsts.updated(idx, originalInsts(idx)) == originalInsts)

      val metadataDrifts = List(
        target.copy(main = true),
        target.copy(kind = FuncKind.Aux),
        target.copy(name = s"$name.drift"),
        target.copy(params = target.params.dropRight(1)),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(lhs = Name("drift")),
          ),
        ),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(optional = true),
          ),
        ),
        target.copy(params =
          target.params.updated(
            0,
            target.params.head.copy(ty = Type(MathT)),
          ),
        ),
        target.copy(retTy = Type(MathT)),
      )
      for (drift <- metadataDrifts) {
        assertThrows[FVExport.Unsupported] {
          FVExport.normalizeIntegralFloorDivision(drift)
        }
      }

      val malformedPrerequisite = target.copy(
        body = ISeq(originalInsts.updated(idx - 1, INop())),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeIntegralFloorDivision(malformedPrerequisite)
      }
      val removedTarget = target.copy(
        body = ISeq(originalInsts.updated(idx, INop())),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeIntegralFloorDivision(removedTarget)
      }

      val originalExpr = originalInsts(idx) match
        case ILet(Name("middle"), expr) => expr
        case IReturn(expr)              => expr
        case other                      => fail(s"unexpected target $other")
      val replacementExpr = normalizedInsts(idx) match
        case ILet(Name("middle"), expr) => expr
        case IReturn(expr)              => expr
        case other                      => fail(s"unexpected target $other")
      val numerator =
        if (name.endsWith("prototype.reverse")) ERef(Name("len"))
        else
          EBinary(
            BOp.Sub,
            ERef(Name("byteLength")),
            ERef(Name("byteOffset")),
          )
      val denominator =
        if (name.endsWith("prototype.reverse")) EMath(2)
        else ERef(Name("elementSize"))
      assert(
        originalExpr ==
        FVExport.integralFloorDivOriginal(numerator, denominator),
      )
      assert(
        replacementExpr ==
        FVExport.integralFloorDivReplacement(numerator, denominator),
      )
      val nestedOriginal = IIf(
        EBool(true),
        ISeq(List(IExpr(originalExpr))),
        ISeq(Nil),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeIntegralFloorDivision(
          target.copy(body = ISeq(originalInsts :+ nestedOriginal)),
        )
      }
      val nestedReplacement = IIf(
        EBool(true),
        ISeq(List(IExpr(replacementExpr))),
        ISeq(Nil),
      )
      assertThrows[FVExport.Unsupported] {
        FVExport.normalizeIntegralFloorDivision(
          target.copy(body = ISeq(originalInsts :+ nestedReplacement)),
        )
      }

      val unrelated = target.copy(name = s"$name.unrelated")
      assert(FVExport.normalizeForRocq(unrelated) eq unrelated)
    }
  }

  test("emit PromiseAllResolveElementFunction.Index as nonnegative Math") {
    val decls =
      FVTyModel.readDecls(s"$BASE_DIR/logs/dump/debugger/tyModel.decls.json")
    val indexTy =
      TyModel(decls)
        .ownFieldsOf("PromiseAllResolveElementFunction")("Index")
        .value

    assert(
      FVTyModel.fieldConstraint(indexTy) ==
      "(RFCMathIntSign false true true)",
    )
  }

  test(
    "normalize PropertyDescriptor key iteration structurally and fail closed",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val target = cfg.program.funcs
      .find(_.name == "ValidateAndApplyPropertyDescriptor")
      .get

    assert(FVExport.countDescKeys(target.body) == 5)
    val normalized = FVExport.normalizeForRocq(target)
    assert(FVExport.countDescKeys(normalized.body) == 0)
    assert(
      FVExport.propertyDescriptorFields ==
      List(
        "Value",
        "Writable",
        "Get",
        "Set",
        "Enumerable",
        "Configurable",
      ),
    )
    val copied = scala.collection.mutable.Map.empty[String, Int]
    val copyScan = new esmeta.ir.util.UnitWalker {
      override def walk(inst: Inst): Unit = inst match
        case IAssign(
              Field(
                Field(
                  Field(Name("O"), EStr("__MAP__")),
                  ERef(Name("P")),
                ),
                EStr(to),
              ),
              ERef(Field(Name("Desc"), EStr(from))),
            ) if to == from =>
          copied(to) = copied.getOrElse(to, 0) + 1
          super.walk(inst)
        case _ => super.walk(inst)
    }
    copyScan.walk(normalized.body)
    assert(
      copied.toMap ==
      FVExport.propertyDescriptorFields.map(_ -> 4).toMap,
    )
    val emitted = FVExport.rocqFunc(target)
    assert(!emitted.contains("""EKeys (ERef (RVar (VLocal (LName "Desc"))))"""))

    val unrelated = cfg.program.funcs.find(_.name == "IsDataDescriptor").get
    assert(FVExport.normalizeForRocq(unrelated) eq unrelated)

    var changed = false
    val drift = new esmeta.ir.util.Walker {
      override def walk(expr: Expr): Expr = expr match
        case EKeys(ERef(Name("Desc")), false) if !changed =>
          changed = true
          EKeys(ERef(Name("Desc")), true)
        case _ => super.walk(expr)
    }
    val drifted = target.copy(body = drift.walk(target.body))
    val error = intercept[FVExport.Unsupported] {
      FVExport.normalizeForRocq(drifted)
    }
    assert(error.msg.contains("normalization shape drift"))
  }

  test(
    "normalize Number::remainder integer suffix once and fail closed",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val remainders = cfg.program.funcs.filter(_.name == "Number::remainder")
    assert(remainders.size == 1)
    val target = remainders.head
    val originalSuffix = FVExport.numberRemainderOriginalSuffix
    val replacementSuffix = FVExport.numberRemainderNormalizedSuffix

    assert(
      FVExport.countInstSuffixOccurrences(target.body, originalSuffix) == 1,
    )
    assert(
      FVExport.countInstSuffixOccurrences(target.body, replacementSuffix) == 0,
    )
    val originalInsts = target.body match
      case ISeq(insts) => insts
      case other       => fail(s"expected top-level ISeq, got $other")
    assert(originalInsts.endsWith(originalSuffix))

    val normalized = FVExport.normalizeForRocq(target)
    val expected = target.copy(
      body = ISeq(
        originalInsts.dropRight(originalSuffix.length) ::: replacementSuffix,
      ),
    )
    assert(normalized == expected)
    assert(
      FVExport.countInstSuffixOccurrences(normalized.body, originalSuffix) == 0,
    )
    assert(
      FVExport.countInstSuffixOccurrences(
        normalized.body,
        replacementSuffix,
      ) == 1,
    )

    val n = ERef(Name("n"))
    val d = ERef(Name("d"))
    assert(
      FVExport.countExprOccurrences(
        normalized.body,
        EConvert(COp.ToMath, n),
      ) == 1,
    )
    assert(
      FVExport.countExprOccurrences(
        normalized.body,
        EConvert(COp.ToMath, d),
      ) == 1,
    )
    val nMath = ERef(Name("nM"))
    val dMath = ERef(Name("dM"))
    val r = ERef(Name("r"))
    val nNegative = EBinary(BOp.Lt, n, ENumber(-0.0))
    val dNegative = EBinary(BOp.Lt, d, ENumber(-0.0))
    assert(
      replacementSuffix.slice(1, 4) == List(
        ILet(Name("nM"), EConvert(COp.ToMath, n)),
        ILet(Name("dM"), EConvert(COp.ToMath, d)),
        ILet(Name("r"), EBinary(BOp.Mod, nMath, dMath)),
      ),
    )
    assert(
      replacementSuffix(4) == IIf(
        EBinary(
          BOp.And,
          EUnary(UOp.Not, EBinary(BOp.Equal, r, EMath(0))),
          EBinary(
            BOp.Or,
            EBinary(BOp.And, nNegative, EUnary(UOp.Not, dNegative)),
            EBinary(BOp.And, EUnary(UOp.Not, nNegative), dNegative),
          ),
        ),
        IAssign(Name("r"), EBinary(BOp.Sub, r, dMath)),
        ISeq(Nil),
      ),
    )

    // Z.modulo follows the divisor.  The replacement subtracts that divisor
    // exactly when a nonzero result has operands of opposite signs, yielding
    // the dividend-sign truncated remainder required by Number::remainder.
    def normalizedIntegerRemainder(n: BigInt, d: BigInt): BigInt = {
      val floored = ((n % d) + d) % d
      if (floored != 0 && n.signum != d.signum) floored - d else floored
    }
    assert(normalizedIntegerRemainder(-5, 2) == -1)
    assert(normalizedIntegerRemainder(5, -2) == 1)
    assert(normalizedIntegerRemainder(-5, -2) == -1)
    assert(normalizedIntegerRemainder(-4, 2) == 0)
    val signedZeroBranch = replacementSuffix(replacementSuffix.length - 2)
    assert(
      signedZeroBranch == IIf(
        EBinary(
          BOp.And,
          EBinary(BOp.Equal, r, EMath(0)),
          EBinary(BOp.Lt, n, ENumber(-0.0)),
        ),
        ISeq(List(IReturn(ENumber(-0.0)))),
        ISeq(Nil),
      ),
    )

    // Fractional inputs are still rejected by the retained ToMath boundary;
    // the normalizer does not introduce raw Number BMod semantics.
    assert(!replacementSuffix.exists {
      case IReturn(EBinary(BOp.Mod, _, _)) => true
      case _                               => false
    })

    val drifted = target.copy(
      body = ISeq(
        originalInsts.updated(originalInsts.length - 1, IReturn(EMath(0))),
      ),
    )
    val error = intercept[FVExport.Unsupported] {
      FVExport.normalizeForRocq(drifted)
    }
    assert(error.msg.contains("Number::remainder"))
    assert(error.msg.contains("normalization shape drift"))
    assert(error.msg.contains("original suffixes=0->0"))

    val differentlyNamed = target.copy(name = "Number::remainder.drift")
    assert(FVExport.normalizeForRocq(differentlyNamed) eq differentlyNamed)
  }

  test(
    "normalize BigInt.asIntN zero-width threshold once and fail closed",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val asIntNs =
      cfg.program.funcs.filter(_.name == "INTRINSICS.BigInt.asIntN")
    assert(asIntNs.size == 1)
    val target = asIntNs.head

    val bits = ERef(Name("bits"))
    val bigint = ERef(Name("bigint"))
    val mod = ERef(Name("mod"))
    val expectedModulo =
      EBinary(
        BOp.Mod,
        EConvert(COp.ToMath, bigint),
        EBinary(BOp.Pow, EMath(2), bits),
      )
    val expectedThreshold =
      EUnary(
        UOp.Not,
        EBinary(
          BOp.Lt,
          mod,
          EBinary(
            BOp.Pow,
            EMath(2),
            EBinary(BOp.Sub, bits, EMath(1)),
          ),
        ),
      )
    val expectedGuard =
      EBinary(
        BOp.And,
        EBinary(BOp.Lt, EMath(0), bits),
        expectedThreshold,
      )

    assert(FVExport.asIntNModuloExpr == expectedModulo)
    assert(FVExport.asIntNThresholdCondition == expectedThreshold)
    assert(FVExport.asIntNGuardedCondition == expectedGuard)

    val originalInsts = target.body match
      case ISeq(insts) => insts
      case other       => fail(s"expected top-level ISeq, got $other")

    val exactPairs =
      originalInsts
        .sliding(2)
        .zipWithIndex
        .collect {
          case (
                List(
                  ILet(Name("mod"), expr),
                  branch @ IIf(cond, _, _, _),
                ),
                idx,
              ) if expr == expectedModulo && cond == expectedThreshold =>
            idx -> branch
        }
        .toList
    assert(exactPairs.size == 1)
    val (pairIndex, originalBranch) = exactPairs.head

    val toIndexCalls = originalInsts.zipWithIndex.collect {
      case (
            ICall(temp, EClo("ToIndex", Nil), List(arg)),
            idx,
          ) if arg == bits =>
        idx -> temp
    }
    val toBigIntCalls = originalInsts.zipWithIndex.collect {
      case (
            ICall(temp, EClo("ToBigInt", Nil), List(arg)),
            idx,
          ) if arg == bigint =>
        idx -> temp
    }
    assert(toIndexCalls.size == 1)
    assert(toBigIntCalls.size == 1)
    val (toIndexIndex, bitsTemp) = toIndexCalls.head
    val (toBigIntIndex, _) = toBigIntCalls.head
    assert(toIndexIndex < toBigIntIndex)
    assert(toBigIntIndex < pairIndex)

    val bitsAssignIndex = originalInsts.indexWhere {
      case IAssign(Name("bits"), ERef(source)) => source == bitsTemp
      case _                                   => false
    }
    assert(toIndexIndex < bitsAssignIndex)
    assert(bitsAssignIndex < toBigIntIndex)

    val normalized = FVExport.normalizeForRocq(target)
    val expectedNormalized = target.copy(
      body = ISeq(
        originalInsts.updated(
          pairIndex + 1,
          originalBranch.copy(cond = expectedGuard),
        ),
      ),
    )
    assert(normalized == expectedNormalized)
    assert(
      FVExport.countExprOccurrences(normalized.body, expectedThreshold) == 1,
    )
    assert(
      FVExport.countExprOccurrences(normalized.body, expectedGuard) == 1,
    )

    val asUintNs =
      cfg.program.funcs.filter(_.name == "INTRINSICS.BigInt.asUintN")
    assert(asUintNs.size == 1)
    assert(FVExport.normalizeForRocq(asUintNs.head) eq asUintNs.head)

    val drifted = target.copy(
      body = ISeq(
        originalInsts.updated(
          bitsAssignIndex,
          IAssign(Name("bits"), EMath(-1)),
        ),
      ),
    )
    val error = intercept[FVExport.Unsupported] {
      FVExport.normalizeForRocq(drifted)
    }
    assert(error.msg.contains("INTRINSICS.BigInt.asIntN"))
    assert(error.msg.contains("normalization shape drift"))
    assert(error.msg.contains("rewrites=0"))
  }

  test("normalize TimeClip's exact integral boundary and fail closed") {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val targets = cfg.program.funcs.filter(_.name == "TimeClip")
    assert(targets.size == 1)
    val target = targets.head
    val originalInsts = target.body.asInstanceOf[ISeq].insts
    val original =
      FVExport.timeClipLimitCondition(FVExport.timeClipOriginalLimit)
    val replacement = FVExport.timeClipOutOfRangeCondition

    assert(
      FVExport.timeClipOriginalLimit == EBinary(
        BOp.Mul,
        EMath(BigDecimal("8.64")),
        EBinary(BOp.Pow, EMath(10), EMath(15)),
      ),
    )
    assert(
      FVExport.timeClipIntegralLimit ==
      EMath(BigDecimal("8640000000000000")),
    )
    assert(FVExport.countExprOccurrences(target.body, original) == 1)
    val normalized = FVExport.normalizeForRocq(target)
    assert(FVExport.countExprOccurrences(normalized.body, original) == 0)
    assert(FVExport.countExprOccurrences(normalized.body, replacement) == 1)
    assert(!FVExport.rocqFunc(target).contains("8.64"))
    assert(FVExport.rocqFunc(target).contains("8640000000000000"))

    val limit = 8640000000000000.0
    val evalProgram = Program.from("@main def main() = { nop }")
    def outsideTimeClip(number: Double): (Boolean, Int) = {
      val state = State(CFGBuilder(evalProgram))
      state.context.locals += Name("time") -> Number(number)
      val interp = new FVInitState.HostCapturingInterpreter(state)
      val result = interp.eval(replacement) match
        case Bool(value) => value
        case other       => fail(s"expected Bool, got $other")
      result -> interp.capturedHostEntryCount
    }
    assert(outsideTimeClip(limit) == (false, 0))
    assert(outsideTimeClip(-limit) == (false, 0))
    assert(outsideTimeClip(java.lang.Math.nextUp(limit)) == (true, 0))
    assert(outsideTimeClip(java.lang.Math.nextDown(-limit)) == (true, 0))
    assert(outsideTimeClip(java.lang.Math.nextDown(limit)) == (false, 0))
    assert(outsideTimeClip(java.lang.Math.nextUp(-limit)) == (false, 0))
    assert(outsideTimeClip(123.75) == (false, 0))
    assert(outsideTimeClip(-123.75) == (false, 0))

    val invalidGuard = originalInsts.headOption
    assert(invalidGuard.exists {
      case IIf(
            EBinary(
              BOp.Or,
              EBinary(BOp.Eq, ERef(Name("time")), ENumber(nan)),
              EBinary(
                BOp.Or,
                EBinary(BOp.Eq, ERef(Name("time")), ENumber(posInf)),
                EBinary(BOp.Eq, ERef(Name("time")), ENumber(negInf)),
              ),
            ),
            ISeq(List(IReturn(ENumber(result)))),
            ISeq(Nil),
            false,
          ) =>
        nan.isNaN && posInf.isPosInfinity && negInf.isNegInfinity && result.isNaN
      case _ => false
    })

    val drifted = target.copy(body = ISeq(originalInsts.updated(1, INop())))
    val error = intercept[FVExport.Unsupported] {
      FVExport.normalizeTimeClip(drifted)
    }
    assert(error.msg.contains("TimeClip integral boundary"))

    for (
      drift <- List(
        target.copy(main = true),
        target.copy(kind = FuncKind.NumMeth),
        target.copy(params = Nil),
        target.copy(params = target.params.map(_.copy(lhs = Name("drift")))),
        target.copy(params = target.params.map(_.copy(optional = true))),
        target.copy(params = target.params.map(_.copy(ty = Type(MathT)))),
        target.copy(retTy = Type(MathT)),
      )
    ) assertThrows[FVExport.Unsupported](FVExport.normalizeTimeClip(drift))
  }

  test(
    "normalize ToUint8Clamp with clamp, floor, midpoint, and half-even ties",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg
    val targets = cfg.program.funcs.filter(_.name == "ToUint8Clamp")
    assert(targets.size == 1)
    val target = targets.head
    val original = FVExport.toUint8ClampOriginalSuffix
    val replacement = FVExport.toUint8ClampNormalizedSuffix
    val originalInsts = target.body.asInstanceOf[ISeq].insts
    assert(originalInsts.endsWith(original))
    assert(FVExport.countInstSuffixOccurrences(target.body, original) == 1)

    val normalized = FVExport.normalizeForRocq(target)
    val normalizedInsts = normalized.body.asInstanceOf[ISeq].insts
    assert(normalizedInsts.endsWith(replacement))
    assert(FVExport.countInstSuffixOccurrences(normalized.body, original) == 0)
    assert(
      FVExport.countInstSuffixOccurrences(normalized.body, replacement) == 1,
    )
    assert(normalized.usefulYets.isEmpty)
    assert(!FVExport.rocqFunc(target).contains("EYet"))

    var nonIntegralMath = List.empty[BigDecimal]
    val mathScan = new esmeta.ir.util.UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case EMath(n) if !n.isWhole =>
          nonIntegralMath ::= n
          super.walk(expr)
        case _ => super.walk(expr)
    }
    mathScan.walk(normalized.body)
    assert(nonIntegralMath.isEmpty)

    val lowCond = replacement(0).asInstanceOf[IIf].cond
    val highCond = replacement(1).asInstanceOf[IIf].cond
    val rounding = replacement(9).asInstanceOf[IIf]
    val belowCond = rounding.cond
    val aboveBranch = rounding.elseInst.asInstanceOf[IIf]
    val aboveCond = aboveBranch.cond
    val evenBranch = aboveBranch.elseInst.asInstanceOf[IIf]
    val evenCond = evenBranch.cond
    val evalProgram = Program.from("@main def main() = { nop }")

    def evalBool(expr: Expr, number: Double, floor: Int): Boolean = {
      val state = State(CFGBuilder(evalProgram))
      state.context.locals ++= MMap(
        Name("number") -> Number(number),
        Name("f") -> Math(BigDecimal(floor)),
        Name("fNumber") -> Number(floor.toDouble),
        Name("midpoint") -> Number(floor.toDouble + 0.5),
      )
      new FVExport.CapturingInterpreter(state).eval(expr) match
        case Bool(value) => value
        case other       => fail(s"expected Bool, got $other from $expr")
    }

    def rewrittenClamp(number: Double): Int =
      if (number.isNaN) 0 // retained source prefix
      else {
        val floor = scala.math.floor(number.max(0.0).min(255.0)).toInt
        if (evalBool(lowCond, number, floor)) 0
        else if (evalBool(highCond, number, floor)) 255
        else if (evalBool(belowCond, number, floor)) floor
        else if (evalBool(aboveCond, number, floor)) floor + 1
        else if (evalBool(evenCond, number, floor)) floor
        else floor + 1
      }

    val cases = List(
      Double.NaN -> 0,
      Double.NegativeInfinity -> 0,
      -1.0 -> 0,
      -0.0 -> 0,
      0.0 -> 0,
      0.49 -> 0,
      0.5 -> 0,
      0.500001 -> 1,
      1.49 -> 1,
      1.5 -> 2,
      2.5 -> 2,
      3.5 -> 4,
      254.49 -> 254,
      254.5 -> 254,
      254.500001 -> 255,
      255.0 -> 255,
      Double.PositiveInfinity -> 255,
    )
    for ((input, expected) <- cases)
      assert(rewrittenClamp(input) == expected, s"input=$input")

    val drifted = target.copy(
      body = ISeq(originalInsts.updated(originalInsts.length - 1, INop())),
    )
    val error = intercept[FVExport.Unsupported] {
      FVExport.normalizeToUint8Clamp(drifted)
    }
    assert(error.msg.contains("ToUint8Clamp half-even"))

    for (
      drift <- List(
        target.copy(main = true),
        target.copy(kind = FuncKind.NumMeth),
        target.copy(params = Nil),
        target.copy(params = target.params.map(_.copy(lhs = Name("drift")))),
        target.copy(params = target.params.map(_.copy(optional = true))),
        target.copy(params = target.params.map(_.copy(ty = Type(MathT)))),
        target.copy(retTy = Type(MathT)),
      )
    ) assertThrows[FVExport.Unsupported](FVExport.normalizeToUint8Clamp(drift))
  }

  test(
    "Spec scan observes exporter normalization but keeps random unsupported",
  ) {
    val cfg = CFGBuilder(Compiler(Extractor()))
    given esmeta.cfg.CFG = cfg

    for (name <- List("TimeClip", "ToUint8Clamp")) {
      val target = cfg.program.funcs.filter(_.name == name)
      assert(target.size == 1)
      assert(FVSpecScan.blockers(target.head).isEmpty, name)
    }

    val randoms =
      cfg.program.funcs.filter(_.name == "INTRINSICS.Math.random")
    assert(randoms.size == 1)
    val randomBlockers = FVSpecScan.blockers(randoms.head)
    assert(randomBlockers.contains("expr:ERandom"))
    assert(!randomBlockers.contains("PHANTOM"))
  }
}
