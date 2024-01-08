package esmeta.lang

import esmeta.lang.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** stringify test */
class StringifyTinyTest extends LangTest {
  import LangTest.*

  val name: String = "langStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // blocks
    // -------------------------------------------------------------------------
    checkParseAndStringify("Block", Block)(
      stepBlock -> """
      |  1. Let _x_ be _x_.
      |  1. [id="x,y,z"] Let _x_ be _x_.
      |  1. Let _x_ be _x_.""".stripMargin,
      exprBlock -> """
      |  * _x_
      |  * _x_
      |  * _x_""".stripMargin,
      figureBlock -> """
      |  <figure>
      |    a
      |    b
      |    c
      |  </figure>""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // algorithm steps
    // -------------------------------------------------------------------------
    checkParseAndStringify("Step", Step)(
      letStep -> "let _x_ be _x_.",
      toBlockStep(letStepClosure) -> """
      |  1. Let _x_ be a new Abstract Closure with parameters (_x_, _x_) that captures _x_ and performs the following steps when called:
      |    1. Let _x_ be _x_.""".stripMargin,
      setStep -> "set _x_ to _x_ + _x_.",
      setFieldsWithIntrinsicsStep -> "set fields of _x_ with the values listed in <emu-xref href=\"#table-well-known-intrinsic-objects\"></emu-xref>.",
      ifStep -> "if _x_ < _x_ + _x_, let _x_ be _x_.",
      toBlockStep(ifBlockStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfElseStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      returnStep -> "return _x_.",
      returnStepNoExpr -> "return.",
      assertStep -> "assert: _x_ and _x_.",
      forEachStep -> "for each Base _x_ of _x_, let _x_ be _x_.",
      forEachReverseStep -> "for each Base _x_ of _x_, in reverse List order, let _x_ be _x_.",
      forEachStepNoType -> "for each _x_ of _x_, let _x_ be _x_.",
      forEachIntStepTrue -> (
        "for each integer _x_ such that 2 ≤ _x_ ≤ 5, in ascending order, " +
        "let _x_ be _x_."
      ),
      forEachIntStepFalse -> (
        "for each integer _x_ such that 2 ≤ _x_ ≤ 5, in descending order, " +
        "let _x_ be _x_."
      ),
      toBlockStep(forEachArrayIndexStep) -> """
      |  1. For each own property key _x_ of _x_ that is an array index, whose numeric value is greater than or equal to _x_, in descending numeric index order, do
      |    1. Let _x_ be _x_.""".stripMargin,
      throwStep -> "throw a newly created *TypeError* object.",
      performStep -> "perform ToObject(_x_ + _x_, -_x_).",
      appendStep -> "append _x_ to _x_.[[Value]].",
      prependStep -> "prepend _x_ to _x_.[[Value]].",
      repeatStep -> "repeat, let _x_ be _x_.",
      repeatCondStep -> """repeat, while _x_ and _x_,
      |  1. Let _x_ be _x_.""".stripMargin,
      pushCtxtStep -> ("push _x_ onto the execution context stack; " +
      "_x_ is now the running execution context."),
      noteStep -> "NOTE: At this point, it must be a numeric operation.",
      suspendStep -> "suspend _x_.",
      suspendAndRemoveStep -> "suspend _x_ and remove it from the execution context stack.",
      removeElemStep -> "remove _x_ from _x_.",
      removeFirstStep -> "remove the first element from _x_.",
      removeCtxtStep -> "remove _x_ from the execution context stack and restore the execution context that is at the top of the execution context stack as the running execution context.",
      removeCtxtWithRestoreStep -> "remove _x_ from the execution context stack and restore _x_ as the running execution context.",
      toBlockStep(setEvalStateStep) -> """
      |  1. Set the code evaluation state of _x_ such that when evaluation is resumed for that execution context the following steps will be performed:
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(setEvalStateParamStep) -> """
      |  1. Set the code evaluation state of _x_ such that when evaluation is resumed with a _x_ the following steps will be performed:
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeArgStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta> using _x_ as the result of the operation that suspended it.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeParamStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>. Let _x_ be the value returned by the resumed computation.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeYieldStep) -> """
      |  1. Resume _x_ passing _x_. If _x_ is ever resumed again, let _x_ be the Completion Record with which it is resumed.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(returnToResumeStep) -> """
      |  1. Return _x_.
      |  1. NOTE: This returns to the evaluation of the operation that had most previously resumed evaluation of _x_.""".stripMargin,
      blockStep -> """
      |  1. Let _x_ be _x_.""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // algorithm expressions
    // -------------------------------------------------------------------------
    checkParseAndStringify("Expression", Expression)(
      refExpr -> "_x_",
      stringConcatExprOne -> "the string-concatenation of _x_",
      stringConcatExprTwo -> "the string-concatenation of _x_ and _x_",
      stringConcatExprThree -> "the string-concatenation of _x_, _x_, and _x_",
      listConcatExprOne -> "the list-concatenation of _x_",
      listConcatExprTwo -> "the list-concatenation of _x_ and _x_",
      listConcatExprThree -> "the list-concatenation of _x_, _x_, and _x_",
      recordEmptyExpr -> "Object { }",
      recordExpr -> "Object { [[Value]]: _x_ }",
      lengthExpr -> "the length of _x_",
      substrExpr -> "the substring of _x_ from _x_",
      substrExprTo -> "the substring of _x_ from _x_ to _x_",
      trim -> "the String value that is a copy of _x_ with both leading and trailing white space removed",
      trimStart -> "the String value that is a copy of _x_ with leading white space removed",
      trimEnd -> "the String value that is a copy of _x_ with trailing white space removed",
      numberOfExpr -> "the number of elements in _x_",
      sourceTextExpr -> "the source text matched by |Identifier|",
      coveredByExpr -> "the |Identifier| that is covered by |Identifier|",
      getItemsExpr -> "the List of |Identifier| items in _x_, in source text order",
      intrExpr -> "%Array%",
      invokeAOExpr -> "ToObject(_x_ + _x_, -_x_)",
      invokeNumericExpr -> "Number::add(_x_, _x_)",
      invokeClosureExpr -> "_x_(_x_)",
      invokeMethodExpr -> "_x_.[[Value]](_x_ + _x_, -_x_)",
      invokeSDOExprZero -> "StringValue of |Identifier|",
      invokeSDOExprSingle -> ("StringValue of |Identifier| " +
      "with argument |Identifier|"),
      invokeSDOExprMulti -> ("StringValue of |Identifier| " +
      "with arguments |Identifier| and _x_"),
      invokeSDOExprEval -> "the result of evaluating |Identifier|",
      invokeSDOExprContains -> "|Identifier| Contains _x_",
      riaCheckExpr -> "? ToObject(_x_ + _x_, -_x_)",
      riaNoCheckExpr -> "! ToObject(_x_ + _x_, -_x_)",
      emptyListExpr -> "« »",
      listExpr -> "« _x_, _x_ »",
      xrefAlgoExpr -> "the definition specified in <emu-xref href=\"#sec-x\"></emu-xref>",
      xrefSlotsExpr -> "the internal slots listed in <emu-xref href=\"#sec-x\"></emu-xref>",
      xrefLenExpr -> "the number of non-optional parameters of the function definition in <emu-xref href=\"#sec-x\"></emu-xref>",
      soleExpr -> "the sole element of « _x_, _x_ »",
      codeUnitAtExpr -> "the code unit at index _x_ within _x_",
    )

    // -------------------------------------------------------------------------
    // algorithm calculation expressions
    // -------------------------------------------------------------------------
    checkParseAndStringify("CalcExpression", Expression)(
      minExpr -> "min(_x_)",
      addExpr -> "_x_ + _x_",
      subExpr -> "_x_ - _x_",
      mulExpr -> "_x_ × _x_",
      expExpr -> "_x_<sup>_x_</sup>",
      unExpr -> "-_x_",
      parenAddExpr -> "_x_ × (_x_ + _x_)",
      parenMulExpr -> "-(_x_ × _x_)",
      parenUnExpr -> "(-_x_)<sup>_x_</sup>",
      convToApproxNumberExpr -> "an implementation-approximated Number value representing _x_",
      convToNumberTextExpr -> "the Number value of the code unit at index _x_ within _x_",
      convToBigIntTextExpr -> "the BigInt value of the code unit at index _x_ within _x_",
      convToMathTextExpr -> "the numeric value of the code unit at index _x_ within _x_",
      convToNumberExpr -> "𝔽(_x_)",
      convToBigIntExpr -> "ℤ(_x_)",
      convToMathExpr -> "ℝ(_x_)",
    )
    // -------------------------------------------------------------------------
    // algorithm mathematical operation expressions
    // -------------------------------------------------------------------------
    checkParseAndStringify("MathOpExpression", Expression)(
      MathOpExpression(
        MathOpExpressionOperator.Neg,
        List(refExpr),
      ) -> "the negation of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Add,
        List(refExpr, refExpr),
      ) -> "the sum of _x_ and _x_",
      MathOpExpression(
        MathOpExpressionOperator.Mul,
        List(refExpr, refExpr),
      ) -> "the product of _x_ and _x_",
      MathOpExpression(
        MathOpExpressionOperator.Sub,
        List(refExpr, refExpr),
      ) -> "the difference _x_ minus _x_",
      MathOpExpression(
        MathOpExpressionOperator.Pow,
        List(refExpr, refExpr),
      ) -> "the raising _x_ to the _x_ power",
      MathOpExpression(
        MathOpExpressionOperator.Expm1,
        List(refExpr),
      ) -> "the subtracting 1 from the exponential function of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Log10,
        List(refExpr),
      ) -> "the base 10 logarithm of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Log2,
        List(refExpr),
      ) -> "the base 2 logarithm of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Cos,
        List(refExpr),
      ) -> "the cosine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Cbrt,
        List(refExpr),
      ) -> "the cube root of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Exp,
        List(refExpr),
      ) -> "the exponential function of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Cosh,
        List(refExpr),
      ) -> "the hyperbolic cosine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Sinh,
        List(refExpr),
      ) -> "the hyperbolic sine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Tanh,
        List(refExpr),
      ) -> "the hyperbolic tangent of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Acos,
        List(refExpr),
      ) -> "the inverse cosine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Acosh,
        List(refExpr),
      ) -> "the inverse hyperbolic cosine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Asinh,
        List(refExpr),
      ) -> "the inverse hyperbolic sine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Atanh,
        List(refExpr),
      ) -> "the inverse hyperbolic tangent of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Asin,
        List(refExpr),
      ) -> "the inverse sine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Atan2,
        List(refExpr, refExpr),
      ) -> "the inverse tangent of the quotient _x_ / _x_",
      MathOpExpression(
        MathOpExpressionOperator.Atan,
        List(refExpr),
      ) -> "the inverse tangent of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Log1p,
        List(refExpr),
      ) -> "the natural logarithm of 1 + _x_",
      MathOpExpression(
        MathOpExpressionOperator.Log,
        List(refExpr),
      ) -> "the natural logarithm of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Sin,
        List(refExpr),
      ) -> "the sine of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Sqrt,
        List(refExpr),
      ) -> "the square root of _x_",
      MathOpExpression(
        MathOpExpressionOperator.Tan,
        List(refExpr),
      ) -> "the tangent of _x_",
    )

    // -------------------------------------------------------------------------
    // algorithm literals
    // -------------------------------------------------------------------------
    checkParseAndStringify("Literal", Expression)(
      ThisLiteral() -> "*this* value",
      NewTargetLiteral() -> "NewTarget",
      hex -> "0x0024",
      hexWithName -> "0x0024 (DOLLAR SIGN)",
      code -> "`|`",
      nt -> "|Identifier|",
      firstNt -> "the first |Identifier|",
      secondNt -> "the second |Identifier|",
      ntFlags -> "|A[~Yield, +Await]|",
      empty -> "~empty~",
      emptyStr -> """*""*""",
      str -> """*"abc"*""",
      strWithStar -> """*"abc\*"*""",
      strWithBasckSlash -> """*"abc\\"*""",
      fieldLit -> "[[Value]]",
      sym -> "@@iterator",
      errObj -> "a newly created *TypeError* object",
      PositiveInfinityMathValueLiteral() -> "+∞",
      NegativeInfinityMathValueLiteral() -> "-∞",
      mathVal -> "0.5",
      mathPi -> "π",
      mathPiWithPre -> "2π",
      posZero -> "*+0*<sub>𝔽</sub>",
      negZero -> "*-0*<sub>𝔽</sub>",
      posInf -> "*+∞*<sub>𝔽</sub>",
      negInf -> "*-∞*<sub>𝔽</sub>",
      nan -> "*NaN*",
      number -> "*1*<sub>𝔽</sub>",
      bigint -> "*1000000000000000000000000*<sub>ℤ</sub>",
      TrueLiteral() -> "*true*",
      FalseLiteral() -> "*false*",
      UndefinedLiteral() -> "*undefined*",
      NullLiteral() -> "*null*",
      AbsentLiteral() -> "absent",
      UndefinedTypeLiteral() -> "Undefined",
      NullTypeLiteral() -> "Null",
      BooleanTypeLiteral() -> "Boolean",
      StringTypeLiteral() -> "String",
      SymbolTypeLiteral() -> "Symbol",
      NumberTypeLiteral() -> "Number",
      BigIntTypeLiteral() -> "BigInt",
      ObjectTypeLiteral() -> "Object",
    )

    // -------------------------------------------------------------------------
    // algorithm clamp expressions
    // -------------------------------------------------------------------------
    checkParseAndStringify("ClampExpression", Expression)(
      clampExpr -> "the result of clamping _x_ between _x_ and _x_",
    )

    // -------------------------------------------------------------------------
    // algorithm bitwise expressions
    // -------------------------------------------------------------------------
    checkParseAndStringify("BitwiseExpression", Expression)(
      bAndExpr -> "the result of applying the bitwise AND operation to _x_ and _x_",
      bXorExpr -> "the result of applying the bitwise exclusive OR (XOR) operation to _x_ and _x_",
      bOrExpr -> "the result of applying the bitwise inclusive OR operation to _x_ and _x_",
    )

    // -------------------------------------------------------------------------
    // algorithm conditions
    // -------------------------------------------------------------------------
    checkParseAndStringify("Condition", Condition)(
      exprCond -> "_x_",
      instanceOfCond -> "_x_ is a Base",
      notInstanceOfCond -> "_x_ is not a Base",
      eitherInstanceOfCond -> "_x_ is either a Base, a Base, or a Base",
      neitherInstanceOfCond -> "_x_ is neither a Base nor a Base",
      hasFieldCond -> "_x_ has a [[Value]] internal slot",
      noHasFieldCond -> "_x_ does not have a [[Value]] internal slot",
      hasBindingCond -> "_x_ has a binding for _x_",
      noHasBindingCond -> "_x_ does not have a binding for _x_",
      prodCond -> "|Identifier| is <emu-grammar>Identifier : Identifier</emu-grammar>",
      finiteCond -> "_x_ is finite",
      abruptCond -> "_x_ is an abrupt completion",
      normalCond -> "_x_ is a normal completion",
      dupCond -> "_x_ is duplicate entries",
      presentCond -> "_x_ is present",
      emptyCond -> "_x_ is empty",
      strictCond -> "_x_ is strict mode code",
      arrayIndexCond -> "_x_ is an array index",
      nonNegativeCond -> "_x_ is a non-negative integral Number",
      integralCond -> "_x_ is an integral Number",
      isCond -> "_x_ is the length of _x_",
      areCond -> "both _x_ and _x_ are not *true*",
      isEitherCond -> "_x_ is either *true* or *false*",
      isNeitherCond -> "_x_ is neither *true* nor *false*",
      binaryCondLt -> "_x_ < _x_ + _x_",
      inclusiveIntervalCond -> "_x_ is in the inclusive interval from 2 to 32",
      notInclusiveIntervalCond -> "_x_ is not in the inclusive interval from 2 to 32",
      containsWhoseCond -> "_x_ contains a Base whose [[Value]] is _x_",
      compCond -> "_x_ and _x_",
      implyCond -> "If _x_ is the length of _x_, then _x_ is either *true* or *false*",
    )

    // -------------------------------------------------------------------------
    // algorithm references
    // -------------------------------------------------------------------------
    checkParseAndStringify("Reference", Reference)(
      x -> "_x_",
      RunningExecutionContext() -> "the running execution context",
      SecondExecutionContext() -> "the second to top element of the execution context stack",
      CurrentRealmRecord() -> "the current Realm Record",
      ActiveFunctionObject() -> "the active function object",
      fieldRef -> "_x_.[[Value]]",
      intrFieldRef -> "_x_.[[%Array%]]",
      propIntrFieldRef -> "_x_.[[%Array.prototype.toString%]]",
      componentRef -> "_x_.Realm",
      indexRef -> "_x_[_x_]",
      ntRef -> "the |Arguments| of _x_",
    )

    // -------------------------------------------------------------------------
    // algorithm references
    // -------------------------------------------------------------------------
    checkParseAndStringify("Property", Property)(
      fieldProp -> ".[[Value]]",
      componentProp -> ".Realm",
      bindingProp -> "the binding for _x_ in",
      indexProp -> "[_x_]",
      intrProp -> ".[[%Array%]]",
      propIntrProp -> ".[[%Array.prototype.toString%]]",
    )

    // -------------------------------------------------------------------------
    // algorithm intrinsics
    // -------------------------------------------------------------------------
    checkParseAndStringify("Intrinsic", Intrinsic)(
      intr -> "%Array%",
      propIntr -> "%Array.prototype.toString%",
    )

    // -------------------------------------------------------------------------
    // algorithm types
    // -------------------------------------------------------------------------
    checkParseAndStringify("Type", Type)(
      Type(NumberT) -> "a Number",
      Type(BigIntT) -> "a BigInt",
      Type(BoolT) -> "a Boolean",
      Type(SymbolT) -> "a Symbol",
      Type(StrT) -> "a String",
      Type(ObjectT) -> "an Object",
      Type(ESValueT) -> "an ECMAScript language value",
      Type(TrueT) -> "*true*",
      Type(FalseT) -> "*false*",
      Type(UndefT) -> "*undefined*",
      Type(NullT) -> "*null*",
      Type(AstT) -> "a Parse Node",
      Type(AstT("Identifier")) -> "an |Identifier| Parse Node",
      Type(ConstT("unused")) -> "~unused~",
      Type(ConstT("string", "symbol")) -> "~string~ or ~symbol~",
      Type(ConstT("start", "end", "start+end")) ->
      "~end~, ~start~, or ~start+end~",
      Type(ListT(NumberT)) -> "a List of Numbers",
      Type(ListT(AstT)) -> "a List of Parse Nodes",
      Type(
        ListT(StrT || UndefT),
      ) -> "a List of either Strings or *undefined*",
      Type(NameT("Environment Record")) -> "an Environment Record",
      Type(NormalT(NumberT)) -> "a normal completion containing a Number",
      Type(NormalT(NumberT || BigIntT)) ->
      "a normal completion containing either a Number or a BigInt",
      Type(AbruptT) -> "an abrupt completion",
      Type(AbruptT("throw")) -> "a throw completion",
      Type(
        AbruptT("return", "throw"),
      ) -> "a return completion or a throw completion",
      Type(NormalT(BigIntT) || AbruptT) ->
      "either a normal completion containing a BigInt or an abrupt completion",
      Type(NormalT(NameT("Property Descriptor")) || AbruptT) ->
      "either a normal completion containing a Property Descriptor or an abrupt completion",
      // more complex
      Type(NumberT || BigIntT) -> "a Number or a BigInt",
      Type(NullT || ESValueT) -> "an ECMAScript language value",
      Type(ESValueT || AstT) ->
      "an ECMAScript language value or a Parse Node",
      Type(
        CompT(ListT(StrT || NullT), Inf),
      ) -> "either a normal completion containing a List of either Strings or *null*, or an abrupt completion",
    )
  }

  init
}
