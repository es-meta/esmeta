package esmeta.compiler

import esmeta.LINE_SEP
import esmeta.ir.{Func, FuncKind, IIf, Inst, Param => IRParam, Type => IRType}
import esmeta.ir.util.{Walker => IRWalker}
import esmeta.lang.*
import esmeta.lang.LangTest.*
import esmeta.spec
import esmeta.spec.{
  AbstractOperationHead,
  Algorithm,
  Constant,
  Grammar,
  Production,
  Spec,
  SyntaxDirectedOperationHead,
  Table,
}
import esmeta.spec.SyntaxDirectedOperationHead.Target
import org.jsoup.nodes.Element
import scala.collection.mutable.{Set => MSet}

/** compilation test for the metalanguage */
class CompileTinyTest extends CompilerTest with SyntaxCoverage {
  val name: String = "compilerCompileTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // steps
    // -------------------------------------------------------------------------
    checkCompileStep("steps")(
      letStep -> "let x = x",
      letCopyStep -> "let x = (copy x)",
      letStepSemicolon -> "let x = x",
      letStepClosure -> """let x = clo<"Test:clo0", [x]>""",
      setStep -> "x = (+ x x)",
      setCopyStep -> "x = (copy x)",
      setAsStep -> """x = clo<"Bar">""",
      setEvalStateStep -> """x.__RESUME_CONT__ = cont<"Test:cont0">""",
      setEvalStateArgStep -> """x.__RESUME_CONT__ = cont<"Test:cont0">""",
      setEvalStateArgsStep -> """x.__RESUME_CONT__ = cont<"Test:cont0">""",
      performStep -> """call %0 = clo<"ToObject">((+ x x), (- x))""",
      invokeShorthandStep ->
      "assert (&& (? x: Completion) (! (= x.Type ~normal~)))",
      appendStep -> "push x.Value < x",
      prependStep -> "push x > x.Value",
      insertStep -> "push x > x.Value",
      addStep -> "push x.Value < x",
      removeStep -> """call %0 = clo<"__REMOVE_ELEM__">(x, x)""",
      // -----------------------------------------------------------------------
      removeFirstStep ->
      """%0 = 0
      |%1 = x
      |while (< %0 %1) {
      |  pop %2 < x
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      removeLastStep -> "pop x > %0",
      pushCtxtStep -> "push x > @EXECUTION_STACK",
      suspendStep -> "nop",
      suspendRefStep -> "nop",
      suspendAndRemoveStep -> "pop %0 < @EXECUTION_STACK",
      removeCtxtStep -> "pop %0 < @EXECUTION_STACK",
      removeCtxtRestoreTopStep -> "pop %0 < @EXECUTION_STACK",
      removeCtxtRestoreStep -> "pop %0 < @EXECUTION_STACK",
      assertStep -> "assert (&& x x)",
      // -----------------------------------------------------------------------
      ifStep ->
      """if x {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifElseInlineStep ->
      """if x {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifElseInlineSemicolonStep ->
      """if x {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifOtherwiseInlineStep ->
      """if x {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifOtherwiseInlineNoCommaStep ->
      """if x {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifBlockStep ->
      """if x {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifElseStep ->
      """if x {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifElseIfStep ->
      """if x {
      |  let x = x
      |} else {
      |  if x {
      |    let x = x
      |  }
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      ifElseIfElseStep ->
      """if x {
      |  let x = x
      |} else {
      |  if x {
      |    let x = x
      |  } else {
      |    let x = x
      |  }
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      repeatStep ->
      """while true {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      repeatWhileStep ->
      """while (&& x x) {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      repeatUntilStep ->
      """while (! (&& x x)) {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachStep ->
      """%1 = x
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  if (? x: Record[Base]) {
      |    let x = x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachReverseStep ->
      """%1 = x
      |%0 = (- (sizeof %1) 1)
      |while (! (< %0 0)) {
      |  let x = %1[%0]
      |  if (? x: Record[Base]) {
      |    let x = x
      |  }
      |  %0 = (- %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachStepNoType ->
      """%1 = x
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  let x = x
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachIntStep ->
      """let x = 2
      |%0 = 6
      |while (! (< %0 x)) {
      |  let x = x
      |  x = (+ x 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachIntNotIncStep ->
      """let x = (- 2 1)
      |%0 = 6
      |while (< x %0) {
      |  let x = x
      |  x = (+ x 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachIntDescStep ->
      """let x = 6
      |%0 = 2
      |while (! (< x %0)) {
      |  let x = x
      |  x = (- x 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachAscOPKStep ->
      """%1 = (keys-int x.__MAP__)
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  if (&& x x) {
      |    let x = x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachDscOPKStep ->
      """%1 = (keys x.__MAP__)
      |%0 = (sizeof %1)
      |while (< 0 %0) {
      |  %0 = (- %0 1)
      |  let x = %1[%0]
      |  if (&& x x) {
      |    let x = x
      |  }
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      forEachParseNodeStep ->
      """%1 = x
      |%0 = 0
      |%2 = (sizeof %1)
      |while (< %0 %2) {
      |  if (exists %1[%0]) {
      |    let x = %1[%0]
      |    let x = x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      returnStep -> "return x",
      // -----------------------------------------------------------------------
      throwStep ->
      """call %0 = clo<"__NEW_ERROR_OBJ__">("%ReferenceError.prototype%")
      |call %1 = clo<"ThrowCompletion">(%0)
      |return %1""".stripMargin,
      // -----------------------------------------------------------------------
      resumeStep ->
      """x.__RESUME_CONT__ = cont<"Test:cont0">
      |pop %0 < x.__RETURN_CONT__
      |call %1 = %0(x)""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalStep ->
      """if (! (exists x.__RETURN_CONT__)) x.__RETURN_CONT__ = (list [])
      |push cont<"Test:cont0"> > x.__RETURN_CONT__
      |call %0 = x.__RESUME_CONT__()""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalArgStep ->
      """if (! (exists x.__RETURN_CONT__)) x.__RETURN_CONT__ = (list [])
      |push cont<"Test:cont0"> > x.__RETURN_CONT__
      |call %0 = x.__RESUME_CONT__(x)""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalParamStep ->
      """if (! (exists x.__RETURN_CONT__)) x.__RETURN_CONT__ = (list [])
      |push cont<"Test:cont0"> > x.__RETURN_CONT__
      |call %0 = x.__RESUME_CONT__()""".stripMargin,
      // -----------------------------------------------------------------------
      resumeTopCtxtStep -> "nop",
      noteStep -> "nop",
      blockStep -> "let x = x",
      yetStep ->
      """(yet "Not yet supported:\n  1. Let _x_ be _x_.\n  1. [id=\"x,y,z\",some-name] Let _x_ be _x_.\n  1. Let _x_ be _x_.")""",
    )

    // -------------------------------------------------------------------------
    // special steps
    // -------------------------------------------------------------------------
    checkCompileStep("special steps")(
      setFieldsWithIntrinsicsStep -> "x = @INTRINSICS",
      // -----------------------------------------------------------------------
      performBlockStep ->
      """let x = x
      |x = (+ x x)""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("expressions")(
      refExpr -> "let x = x",
      stringConcatExprOne -> "let x = (concat x)",
      stringConcatExprTwo -> "let x = (concat x x)",
      stringConcatExprThree -> "let x = (concat x x x)",
      // -----------------------------------------------------------------------
      listConcatExprOne ->
      """call %0 = clo<"__FLAT_LIST__">((list [x]))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      listConcatExprTwo ->
      """call %0 = clo<"__FLAT_LIST__">((list [x, x]))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      listConcatExprThree ->
      """call %0 = clo<"__FLAT_LIST__">((list [x, x, x]))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      listElementsCopyExpr -> "let x = (copy x)",
      copyExpr -> "let x = (copy x)",
      copyOfListExpr -> "let x = (copy x)",
      copyAccessExpr -> "let x = (copy x.Captures)",
      copyRunningContextExpr -> "let x = (copy @EXECUTION_STACK[0])",
      // -----------------------------------------------------------------------
      recordEmptyExpr ->
      """let x = (record [Object] {
      |  "__MAP__" : (map[Record[Symbol] | String, Record[PropertyDescriptor]]),
      |  "PrivateElements" : (list []),
      |})""".stripMargin,
      // -----------------------------------------------------------------------
      recordExpr ->
      """let x = (record [Object] {
      |  "Value" : x,
      |  "__MAP__" : (map[Record[Symbol] | String, Record[PropertyDescriptor]]),
      |  "PrivateElements" : (list []),
      |})""".stripMargin,
      // -----------------------------------------------------------------------
      lengthExpr -> "let x = (sizeof x)",
      substrExpr -> "let x = (substring x x)",
      substrExprTo -> "let x = (substring x x x)",
      trim -> "let x = (trim (trim > x) <)",
      trimStart -> "let x = (trim > x)",
      trimEnd -> "let x = (trim x <)",
      numberOfExpr -> "let x = (sizeof x)",
      numberOfBytesExpr -> "let x = (sizeof x)",
      numberOfListExpr -> "let x = (sizeof x)",
      sourceTextExpr -> "let x = (source-text (grammar-symbol |Identifier|))",
      coveredByExpr ->
      "let x = (parse (grammar-symbol |Identifier|) (grammar-symbol |Identifier|))",
      getItemsExpr ->
      """let x = (yet "the List of |Identifier| items in _x_, in source text order")""",
      intrExpr -> """let x = @EXECUTION_STACK[0].Realm.Intrinsics["%Array%"]""",
      // -----------------------------------------------------------------------
      invokeAOExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeNumericExpr ->
      """call %0 = clo<"Number::add">(x, x)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeClosureExpr ->
      """call %0 = x(x)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeMethodExpr ->
      """call %0 = x.Value(x, (+ x x), (- x))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprZero ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue()
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprSingle ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue((grammar-symbol |Identifier|))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprMulti ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue((grammar-symbol |Identifier|), x)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprEval ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->Evaluation()
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprContains ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->Contains(x)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      riaCheckExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |assert (? %0: Completion)
      |if (? %0: Abrupt) return %0
      |else %0 = %0.Value
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      riaNoCheckExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |assert (? %0: Normal)
      |%0 = %0.Value
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      emptyListExpr -> "let x = (list [])",
      listExpr -> "let x = (list [x, x])",
      xrefAlgoExpr -> """let x = clo<"Foo">""",
      xrefSlotsExpr -> """let x = (list ["Value"])""",
      xrefLenExpr -> "let x = 1",
      // -----------------------------------------------------------------------
      soleExpr ->
      """%0 = (list [x, x])
      |let x = %0[0]""".stripMargin,
      // -----------------------------------------------------------------------
      codeUnitAtExpr -> "let x = x[x]",
      strValueExpr -> "let x = x",
      yetExpr ->
      """let x = (yet "Not yet supported:\n  1. Let _x_ be _x_.\n  1. [id=\"x,y,z\",some-name] Let _x_ be _x_.\n  1. Let _x_ be _x_.")""",
    )

    // -------------------------------------------------------------------------
    // calculation expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("calculation expressions")(
      minExpr -> "let x = (min x)",
      addExpr -> "let x = (+ x x)",
      subExpr -> "let x = (- x x)",
      mulExpr -> "let x = (* x x)",
      expExpr -> "let x = (** x x)",
      unExpr -> "let x = (- x)",
      parenAddExpr -> "let x = (* x (+ x x))",
      parenMulExpr -> "let x = (- (* x x))",
      parenUnExpr -> "let x = (** (- x) x)",
      // -----------------------------------------------------------------------
      addInvokeExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |let x = (+ %0 x)""".stripMargin,
      // -----------------------------------------------------------------------
      mulInvokeExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |let x = (* (** x x) %0)""".stripMargin,
      // -----------------------------------------------------------------------
      plusExpr -> "let x = (+ x x)",
      timesExpr -> "let x = (* x x)",
      // -----------------------------------------------------------------------
      mulSDOExpr ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue()
      |let x = (* %0 x)""".stripMargin,
      // -----------------------------------------------------------------------
      convInvokeExpr ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |let x = ([math] (+ %0 x))""".stripMargin,
      // -----------------------------------------------------------------------
      convToApproxNumberExpr -> "let x = ([approx-number] x)",
      convToNumberTextExpr -> "let x = ([number] x[x])",
      convToBigIntTextExpr -> "let x = ([bigInt] x[x])",
      convToMathTextExpr -> "let x = ([math] x[x])",
      convToNumberExpr -> "let x = ([number] x)",
      convToBigIntExpr -> "let x = ([bigInt] x)",
      convToMathExpr -> "let x = ([math] x)",
    )

    // -------------------------------------------------------------------------
    // mathematical operation expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("mathematical operation expressions")(
      negMathExpr -> "let x = (- x)",
      sumMathExpr -> "let x = (+ x x)",
      prodMathExpr -> "let x = (* x x)",
      diffMathExpr -> "let x = (- x x)",
      powMathExpr -> "let x = (** x x)",
      expm1MathExpr -> "let x = ([math:expm1] x)",
      cosMathExpr -> "let x = ([math:cos] x)",
      cbrtMathExpr -> "let x = ([math:cbrt] x)",
      expMathExpr -> "let x = ([math:exp] x)",
      coshMathExpr -> "let x = ([math:cosh] x)",
      sinhMathExpr -> "let x = ([math:sinh] x)",
      tanhMathExpr -> "let x = ([math:tanh] x)",
      acosMathExpr -> "let x = ([math:acos] x)",
      acoshMathExpr -> "let x = ([math:acosh] x)",
      asinhMathExpr -> "let x = ([math:asinh] x)",
      atanhMathExpr -> "let x = ([math:atanh] x)",
      asinMathExpr -> "let x = ([math:asin] x)",
      atan2MathExpr -> "let x = ([math:atan2] x x)",
      atanMathExpr -> "let x = ([math:atan] x)",
      sinMathExpr -> "let x = ([math:sin] x)",
      sqrtMathExpr -> "let x = ([math:sqrt] x)",
      tanMathExpr -> "let x = ([math:tan] x)",
    )

    // -------------------------------------------------------------------------
    // mathematical function expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("mathematical function expressions")(
      maxExpr -> "let x = (max x x)",
      minTwoExpr -> "let x = (min x x)",
      absExpr -> "let x = (abs x)",
      floorExpr -> "let x = (floor x)",
      log10Expr -> "let x = ([math:log10] x)",
      log2Expr -> "let x = ([math:log2] x)",
      lnExpr -> "let x = ([math:log] x)",
      // -----------------------------------------------------------------------
      truncateExpr ->
      """%0 = x
      |if (< %0 0) %0 = (- (floor (- %0)))
      |else %0 = (floor %0)
      |let x = %0""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // literals
    // -------------------------------------------------------------------------
    checkCompileExpr("literals")(
      thisLit -> "let x = this",
      thisLitWithArticle -> "let x = this",
      thisParseNode -> "let x = this",
      newTarget -> "let x = NewTarget",
      hex -> "let x = 36",
      hexWithName -> "let x = 36cu",
      code -> """let x = "|"""",
      grSym -> "let x = (grammar-symbol |A|)",
      grSymIdx -> "let x = (grammar-symbol |A|[FT])",
      nt -> "let x = (grammar-symbol |Identifier|)",
      firstNt -> "let x = (grammar-symbol |Identifier|)",
      firstNtWithArticle -> "let x = (grammar-symbol |Identifier|)",
      secondNt -> "let x = (grammar-symbol |Identifier|)",
      secondNtWithArticle -> "let x = (grammar-symbol |Identifier|)",
      ntFlags -> "let x = (grammar-symbol |A|[FT])",
      empty -> "let x = ~empty~",
      emptyStr -> """let x = """"",
      str -> """let x = "abc"""",
      strWithStar -> """let x = "abc*"""",
      strWithBasckSlash -> """let x = "abc\\"""",
      fieldLit -> """let x = "Value"""",
      sym -> "let x = @SYMBOL.iterator",
      // -----------------------------------------------------------------------
      errObj ->
      """call %0 = clo<"__NEW_ERROR_OBJ__">("%TypeError.prototype%")
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      mathVal -> "let x = 0.5",
      mathPi -> "let x = @MATH_PI",
      mathPiWithPre -> "let x = (* 2 @MATH_PI)",
      msPerDay -> "let x = 86400000",
      hoursPerDay -> "let x = 24",
      posZero -> "let x = 0.0f",
      negZero -> "let x = -0.0f",
      posInf -> "let x = +NUM_INF",
      negInf -> "let x = -NUM_INF",
      nan -> "let x = NaN",
      number -> "let x = 1.0f",
      bigint -> "let x = 1000000000000000000000000n",
      two -> "let x = 2",
      six -> "let x = 6",
      prodLit -> "let x = |Identifier|<0>",
      posInfMathVal -> "let x = +INF",
      negInfMathVal -> "let x = -INF",
      trueLit -> "let x = true",
      falseLit -> "let x = false",
      undefinedLit -> "let x = undefined",
      nullLit -> "let x = null",
      undefinedTypeLit -> "let x = @Undefined",
      nullTypeLit -> "let x = @Null",
      boolTypeLit -> "let x = @Boolean",
      strTypeLit -> "let x = @String",
      symbolTypeLit -> "let x = @Symbol",
      numberTypeLit -> "let x = @Number",
      bigIntTypeLit -> "let x = @BigInt",
      objectTypeLit -> "let x = @Object",
    )

    // -------------------------------------------------------------------------
    // clamp expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("clamp expressions")(
      clampExpr ->
      """call %0 = clo<"__CLAMP__">(x, x, x)
      |let x = %0""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // bitwise expressions
    // -------------------------------------------------------------------------
    checkCompileExpr("bitwise expressions")(
      bAndExpr -> "let x = (& x x)",
      bXorExpr -> "let x = (^ x x)",
      bOrExpr -> "let x = (| x x)",
    )

    // -------------------------------------------------------------------------
    // conditions
    // -------------------------------------------------------------------------
    checkCompileCond("conditions")(
      exprCond -> "assert x",
      typeCheckCond -> "assert (? x: Record[Base])",
      notTypeCheckCond -> "assert (! (? x: Record[Base]))",
      eitherTypeCheckCond ->
      "assert (|| (|| (? x: Record[Base]) (? x: Record[Base])) (? x: Record[Base]))",
      neitherTypeCheckCond ->
      "assert (! (|| (? x: Record[Base]) (? x: Record[Base])))",
      hasFieldCond -> "assert (exists x.Value)",
      hasMultipleFieldsCond ->
      "assert (&& (&& (exists x.Value) (exists x.Value)) (exists x.Value))",
      noHasFieldCond -> "assert (! (exists x.Value))",
      hasBindingCond -> "assert (exists x.__MAP__[x])",
      noHasBindingCond -> "assert (! (exists x.__MAP__[x]))",
      prodCond ->
      "assert (? (grammar-symbol |Identifier|): Ast[Identifier[0]])",
      finiteCond ->
      "assert (|| (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Math | BigInt))",
      finiteNumberCond ->
      "assert (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN])))",
      finiteNumbersCond ->
      "assert (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))))",
      nonZeroFiniteNumberCond ->
      "assert (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Number[NonZero, -INF, +INF, NaN]))",
      nonZeroFiniteNumbersCond ->
      "assert (&& (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Number[NonZero, -INF, +INF, NaN])) (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Number[NonZero, -INF, +INF, NaN])))",
      abruptCond -> "assert (&& (? x: Completion) (! (= x.Type ~normal~)))",
      normalCond -> "assert (&& (? x: Completion) (= x.Type ~normal~))",
      // -----------------------------------------------------------------------
      dupCond ->
      """call %0 = clo<"__HAS_DUPLICATE__">(x)
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      presentCond -> "assert (exists x)",
      emptyCond -> "assert (= (sizeof x) 0)",
      strictCond -> "assert true",
      // -----------------------------------------------------------------------
      arrayIndexCond ->
      """call %0 = clo<"__IS_ARRAY_INDEX__">(x)
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      isCond -> "assert (= x (sizeof x))",
      areCond -> "assert (&& (! (= x true)) (! (= x true)))",
      isEitherCond -> "assert (|| (= x true) (= x false))",
      isNeitherCond -> "assert (! (|| (= x true) (= x false)))",
      binaryCondLt -> "assert (< x (+ x x))",
      inclusiveIntervalCondShort -> "assert (! (|| (< x 2) (< 32 x)))",
      inclusiveIntervalCond -> "assert (! (|| (< x 2) (< 32 x)))",
      notInclusiveIntervalCond -> "assert (|| (< x 2) (< 32 x))",
      containsCond -> "assert (contains x x)",
      notContainsCond -> "assert (! (contains x x))",
      // -----------------------------------------------------------------------
      containsWhoseFieldCond ->
      """%1 = x
      |%2 = 0
      |%3 = false
      |while (&& (! %3) (< %2 (sizeof %1))) {
      |  %0 = %1[%2]
      |  %3 = (&& (? %0: Record[Base]) (= %0.Field x))
      |  %2 = (+ %2 1)
      |}
      |assert %3""".stripMargin,
      // -----------------------------------------------------------------------
      containsSuchThatCond ->
      """%0 = x
      |%1 = 0
      |%2 = false
      |while (&& (! %2) (< %1 (sizeof %0))) {
      |  let x = %0[%1]
      |  %2 = (&& (? x: Record[Base]) (= x (sizeof x)))
      |  %1 = (+ %1 1)
      |}
      |assert %2""".stripMargin,
      // -----------------------------------------------------------------------
      compCond -> "assert (&& x x)",
      implyCond ->
      "assert (|| (! (= x (sizeof x))) (|| (= x true) (= x false)))",
    )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    checkCompileRef("references")(
      x -> "let x = x",
      xWithNt -> "let x = x",
      access -> "let x = x.Value",
      accessFieldDot -> "let x = x.Value",
      accessCompDot -> "let x = x.Value",
      accessFieldOf -> "let x = x.Value",
      accessCompOf -> "let x = x.Value",
      accessFieldApo -> "let x = x.Value",
      accessCompApo -> "let x = x.Value",
      valueOf -> "let x = x",
      intrField -> """let x = x["%Array%"]""",
      indexLookup -> "let x = x[x]",
      bindingLookup -> "let x = x.__MAP__[x]",
      ntLookup -> "let x = x.Arguments",
      firstElement -> "let x = x[0]",
      // -----------------------------------------------------------------------
      lastElement ->
      """%0 = x
      |let x = %0[(- (sizeof %0) 1)]""".stripMargin,
      // -----------------------------------------------------------------------
      intrObj -> "let x = x.Intrinsics[x]",
      runningExecCtx -> "let x = @EXECUTION_STACK[0]",
      secondExecCtx -> "let x = @EXECUTION_STACK[1]",
      currentRealmRec -> "let x = @EXECUTION_STACK[0].Realm",
      activeFuncObj -> "let x = @EXECUTION_STACK[0].Function",
      agentRec -> "let x = @AGENT_RECORD",
    )

    // -------------------------------------------------------------------------
    // return steps of completion-returning algorithms
    // -------------------------------------------------------------------------
    checkCompileStep("completion-returning algorithms", needRetComp = true)(
      returnStep ->
      """if (? x: Completion) return x
      |call %0 = clo<"NormalCompletion">(x)
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      ReturnStep(trueLit) ->
      """call %0 = clo<"NormalCompletion">(true)
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      ReturnStep(invokeAOExpr) ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |if (? %0: Completion) return %0
      |call %1 = clo<"NormalCompletion">(%0)
      |return %1""".stripMargin,
      // -----------------------------------------------------------------------
      ReturnStep(riaCheckExpr) ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |assert (? %0: Completion)
      |if (? %0: Abrupt) return %0
      |else return %0""".stripMargin,
      // -----------------------------------------------------------------------
      ReturnStep(riaNoCheckExpr) ->
      """call %0 = clo<"ToObject">((+ x x), (- x))
      |assert (? %0: Normal)
      |return %0""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // conditions with short-circuit evaluation
    // -------------------------------------------------------------------------
    checkCompileStep("short-circuit conditions")(
      AssertStep(CompoundCondition(exprCond, And, invokeCond)) ->
      """%0 = x
      |if %0 {
      |  call %1 = clo<"ToObject">((+ x x), (- x))
      |  %0 = %1
      |}
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      AssertStep(CompoundCondition(exprCond, Or, invokeCond)) ->
      """%0 = x
      |if %0 {} else {
      |  call %1 = clo<"ToObject">((+ x x), (- x))
      |  %0 = %1
      |}
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      AssertStep(CompoundCondition(exprCond, Imply, invokeCond)) ->
      """%0 = x
      |if %0 {
      |  call %1 = clo<"ToObject">((+ x x), (- x))
      |  %0 = %1
      |} else %0 = true
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      AssertStep(CompoundCondition(invokeCond, And, invokeCond)) ->
      """call %1 = clo<"ToObject">((+ x x), (- x))
      |%0 = %1
      |if %0 {
      |  call %2 = clo<"ToObject">((+ x x), (- x))
      |  %0 = %2
      |}
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      IfStep(
        CompoundCondition(exprCond, And, invokeCond),
        letStep,
        Some(letStep),
        ElseConfig(),
      ) ->
      """%0 = x
      |if %0 {
      |  call %1 = clo<"ToObject">((+ x x), (- x))
      |  %0 = %1
      |}
      |if %0 {
      |  let x = x
      |} else {
      |  let x = x
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // syntax-directed operations, where a nonterminal is bound to `this`
    // -------------------------------------------------------------------------
    checkCompileExpr("syntax-directed operation expressions", sdo = true)(
      sourceTextExpr -> "let x = (source-text this[0])",
      coveredByExpr -> "let x = (parse this[0] this[0])",
      getItemsExpr ->
      """let x = (yet "the List of |Identifier| items in _x_, in source text order")""",
      // -----------------------------------------------------------------------
      invokeSDOExprZero ->
      """sdo-call %0 = this[0]->StringValue()
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      invokeSDOExprEval ->
      """sdo-call %0 = this[0]->Evaluation()
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      nt -> "let x = this[0]",
    )

    // -------------------------------------------------------------------------
    // conditions of syntax-directed operations
    // -------------------------------------------------------------------------
    checkCompileCond("syntax-directed operation conditions", sdo = true)(
      prodCond -> "assert (? this[0]: Ast[Identifier[0]])",
    )

    // -------------------------------------------------------------------------
    // auxiliary functions (abstract closures and continuations)
    // -------------------------------------------------------------------------
    checkCompileFuncs("auxiliary functions")(
      letStepClosure ->
      """def <CLO>:Test:clo0(
      |  x: Unknown,
      |  x: Unknown,
      |): Unknown = {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      setEvalStateStep ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  call %0 = x()
      |  pop %1 < x.__RETURN_CONT__
      |  call %2 = %1(%0)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      setEvalStateArgStep ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  call %0 = x(x)
      |  pop %1 < x.__RETURN_CONT__
      |  call %2 = %1(%0)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      setEvalStateArgsStep ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  call %0 = x(x, x)
      |  pop %1 < x.__RETURN_CONT__
      |  call %2 = %1(%0)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      resumeStep ->
      """def <CONT>:Test:cont0(
      |  x: Unknown,
      |): Unknown = {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalStep ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalArgStep ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  let x = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      resumeEvalParamStep ->
      """def <CONT>:Test:cont0(
      |  x: Unknown,
      |): Unknown = {
      |  let x = x
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // coverage of the corpus
    // -------------------------------------------------------------------------
    checkCorpusCoverage("corpus")(handled)
  }

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------
  /** shortcuts for the cases depending on the context */
  private val invokeCond = ExpressionCondition(invokeAOExpr)
  private val And = CompoundConditionOperator.And
  private val Or = CompoundConditionOperator.Or
  private val Imply = CompoundConditionOperator.Imply
  private val ElseConfig = IfStep.ElseConfig

  /** an abstract operation of the fixture specification */
  private lazy val fooAlgo: Algorithm =
    val head = AbstractOperationHead(
      false,
      "Foo",
      List(spec.Param("x", UnknownType)),
      UnknownType,
    )
    val algo = Algorithm(head, returnStep, "")
    algo.elem = Element("emu-alg").attr("id", "sec-x")
    algo

  /** an abstract operation referred by `setAsStep` of the corpus */
  private lazy val barAlgo: Algorithm =
    val head = AbstractOperationHead(false, "Bar", Nil, UnknownType)
    val algo = Algorithm(head, returnStep, "")
    algo.elem = Element("emu-alg").attr("id", "id")
    algo

  /** a shorthand referred by `invokeShorthandStep` of the corpus */
  private lazy val shorthandAlgo: Algorithm = Algorithm(
    AbstractOperationHead(
      false,
      "IfAbruptCloseIterator",
      List(
        spec.Param("value", UnknownType),
        spec.Param("iteratorRecord", UnknownType),
      ),
      UnknownType,
    ),
    AssertStep(abruptCond),
    "",
  )

  /** a fixture specification with the minimum for the cases above */
  private lazy val fixture: Spec = Spec(
    grammar = Grammar(List(Production.from("Identifier :\n  Identifier"))),
    algorithms = List(fooAlgo, barAlgo, shorthandAlgo),
    constants = List(
      Constant("msPerDay", DecimalMathValueLiteral(BigDecimal(86400000))),
      Constant("HoursPerDay", DecimalMathValueLiteral(BigDecimal(24))),
    ),
    tables = Map(
      "sec-x" -> Table(
        "sec-x",
        "section X table",
        List("Internal Slot"),
        List(List("[[Value]]")),
      ),
    ),
  )
  private lazy val compiler: Compiler = new Compiler(fixture)

  /** compile a step in a minimal algorithm with the auxiliary functions */
  private def compileStep(
    step: Step,
    needRetComp: Boolean,
    sdo: Boolean,
  ): (Inst, List[Func]) =
    val (kind, head) =
      if (sdo)
        FuncKind.SynDirOp -> SyntaxDirectedOperationHead(
          Some(Target("Identifier", 0, 0)),
          "StringValue",
          false,
          Nil,
          UnknownType,
        )
      else
        FuncKind.AbsOp -> AbstractOperationHead(false, "Test", Nil, UnknownType)
    val fb = FuncBuilder(
      spec = fixture,
      kind = kind,
      name = "Test",
      params = Nil,
      retTy = compiler.compile(UnknownType),
      algo = Algorithm(head, step, ""),
      needRetComp = needRetComp,
    )
    val prevFuncs = compiler.funcs.size
    val inst = compiler.compileWithScope(fb, step)
    (inst, compiler.funcs.drop(prevFuncs).toList)

  /** drop the information that the textual form of IR cannot express */
  private val normalizer = new IRWalker {
    override def walk(ty: IRType): IRType = IRType(ty.ty, None)
    override def walk(param: IRParam): IRParam =
      IRParam(walk(param.lhs), walk(param.ty), param.optional, None)
    override def walk(func: Func): Func = super.walk(func).copy(algo = None)
    override def walk(inst: Inst): Inst = super.walk(inst) match
      case IIf(cond, thenInst, elseInst, _) => IIf(cond, thenInst, elseInst)
      case inst                             => inst
  }

  /** the metalanguage syntax handled by the cases above */
  private val handled: MSet[Syntax] = MSet()

  /** check the IR instructions compiled from metalanguage steps */
  private def checkCompileStep(
    desc: String,
    needRetComp: Boolean = false,
    sdo: Boolean = false,
  )(cases: (Step, String)*): Unit =
    checkCompile(desc)(cases.map((step, expected) => (step, step, expected)))(
      compileStep(_, needRetComp, sdo)._1,
    )

  /** check the IR instructions compiled from metalanguage expressions */
  private def checkCompileExpr(desc: String, sdo: Boolean = false)(
    cases: (Expression, String)*,
  ): Unit =
    checkCompile(desc)(cases.map { (expr, expected) =>
      (expr, LetStep(x, expr), expected)
    })(compileStep(_, false, sdo)._1)

  /** check the IR instructions compiled from metalanguage conditions */
  private def checkCompileCond(desc: String, sdo: Boolean = false)(
    cases: (Condition, String)*,
  ): Unit =
    checkCompile(desc)(cases.map { (cond, expected) =>
      (cond, AssertStep(cond), expected)
    })(compileStep(_, false, sdo)._1)

  /** check the IR instructions compiled from metalanguage references */
  private def checkCompileRef(desc: String)(
    cases: (Reference, String)*,
  ): Unit =
    checkCompile(desc)(cases.map { (ref, expected) =>
      (ref, LetStep(x, ReferenceExpression(ref)), expected)
    })(compileStep(_, false, false)._1)

  /** check the auxiliary IR functions compiled from metalanguage steps */
  private def checkCompileFuncs(desc: String)(
    cases: (Step, String)*,
  ): Unit = check(desc) {
    var failed = 0
    for ((step, expected) <- cases) {
      handled += step
      val funcs = compileStep(step, false, false)._2
      val result = funcs.map(normalizer.walk)
      val expectedFuncs = List(normalizer.walk(Func.from(expected)))
      if (result != expectedFuncs) {
        failed += 1
        println(s"[FAILED] $desc")
        println(s"- syntax: $step")
        println(s"- expected: $expected")
        println(s"- result: ${funcs.mkString(LINE_SEP)}")
      }
    }
    if (failed > 0) fail(s"$failed cases are not compiled as expected")
  }

  /** check the compiled IR of (syntax to cover, step, expected IR) cases */
  private def checkCompile(desc: String)(
    cases: Iterable[(Syntax, Step, String)],
  )(compile: Step => Inst): Unit = check(desc) {
    var failed = 0
    for ((syntax, step, expected) <- cases) {
      handled += syntax
      val inst = compile(step)
      val result = normalizer.walk(inst)
      val expectedInst =
        normalizer.walk(Inst.from(s"{$LINE_SEP$expected$LINE_SEP}"))
      if (result != expectedInst) {
        failed += 1
        println(s"[FAILED] $desc")
        println(s"- syntax: $syntax")
        println(s"- expected: $expected")
        println(s"- result: $inst")
      }
    }
    // NOTE: all the cases are checked before failing to keep the coverage
    if (failed > 0) fail(s"$failed cases are not compiled as expected")
  }

  init
}
