package esmeta.lang

import esmeta.lang.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** stringify test */
class StringifyTinyTest extends LangTest {

  val name: String = "langStringifyTest"

  // registration
  def init: Unit = {
    import LangTest.*

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
      setAsStep -> "set _x_ as specified in <emu-xref href=\"#id\"></emu-xref>.",
      setEvalStateStep -> "set the code evaluation state of _x_ such that when evaluation is resumed for that execution context, _x_ will be called with no arguments.",
      setEvalStateArgStep -> "set the code evaluation state of _x_ such that when evaluation is resumed for that execution context, _x_ will be called with argument _x_.",
      setEvalStateArgsStep -> "set the code evaluation state of _x_ such that when evaluation is resumed for that execution context, _x_ will be called with arguments _x_ and _x_.",
      performStep -> "perform ToObject(_x_ + _x_, -_x_).",
      invokeShorthandStep -> "IfAbruptCloseIterator(_x_, _x_).",
      appendStep -> "append _x_ to _x_.[[Value]].",
      prependStep -> "prepend _x_ to _x_.[[Value]].",
      insertStep -> "insert _x_ as the first element of _x_.[[Value]].",
      addStep -> "add _x_ to _x_.[[Value]].",
      removeStep -> "remove _x_ from _x_.",
      removeFirstStep -> "remove the first _x_ elements from _x_.",
      removeLastStep -> "remove the last element of _x_.",
      pushCtxtStep -> "push _x_ onto the execution context stack; _x_ is now the running execution context.",
      suspendStep -> "suspend the running execution context.",
      suspendRefStep -> "suspend _x_.",
      suspendAndRemoveStep -> "suspend _x_ and remove it from the execution context stack.",
      removeCtxtStep -> "remove _x_ from the execution context stack.",
      removeCtxtRestoreTopStep -> "remove _x_ from the execution context stack and restore the execution context that is at the top of the execution context stack as the running execution context.",
      removeCtxtRestoreStep -> "remove _x_ from the execution context stack and restore _x_ as the running execution context.",
      assertStep -> "assert: _x_ and _x_.",
      ifStep -> "if _x_, let _x_ be _x_.",
      ifElseInlineStep -> "if _x_, let _x_ be _x_. Else, let _x_ be _x_.",
      ifElseInlineSemicolonStep -> "if _x_, let _x_ be _x_; else, let _x_ be _x_.",
      ifOtherwiseInlineStep -> "if _x_, let _x_ be _x_. Otherwise, let _x_ be _x_.",
      ifOtherwiseInlineNoCommaStep -> "if _x_, let _x_ be _x_. Otherwise let _x_ be _x_.",
      toBlockStep(ifBlockStep) -> """
      |  1. If _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseStep) -> """
      |  1. If _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfStep) -> """
      |  1. If _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfElseStep) -> """
      |  1. If _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      repeatStep -> "repeat, let _x_ be _x_.",
      repeatWhileStep -> """repeat, while _x_ and _x_,
      |  1. Let _x_ be _x_.""".stripMargin,
      repeatUntilStep -> """repeat, until _x_ and _x_,
      |  1. Let _x_ be _x_.""".stripMargin,
      forEachStep -> "for each Base _x_ of _x_, let _x_ be _x_.",
      forEachReverseStep -> "for each Base _x_ of _x_, in reverse List order, let _x_ be _x_.",
      forEachStepNoType -> "for each element _x_ of _x_, let _x_ be _x_.",
      forEachIntStep -> "for each integer _x_ such that 2 ≤ _x_ ≤ 6, in ascending order, let _x_ be _x_.",
      forEachIntNotIncStep -> "for each integer _x_ such that 2 < _x_ < 6, in ascending order, let _x_ be _x_.",
      forEachIntDescStep -> "for each integer _x_ such that 2 ≤ _x_ ≤ 6, in descending order, let _x_ be _x_.",
      forEachAscOPKStep -> (
        "for each own property key _x_ of _x_ such that _x_ and _x_, in ascending numeric index order, let _x_ be _x_."
      ),
      forEachDscOPKStep -> (
        "for each own property key _x_ of _x_ such that _x_ and _x_, in descending chronological order of property creation, let _x_ be _x_."
      ),
      returnStep -> "return _x_.",
      throwStep -> "throw a *ReferenceError* exception.",
      toBlockStep(resumeStep) -> """
      |  1. Resume _x_ passing _x_. If _x_ is ever resumed again, let _x_ be the Completion Record with which it is resumed.
      |  1. Let _x_ be _x_.""".stripMargin,
      blockStep -> """
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeEvalStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeEvalArgStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta> using _x_ as the result of the operation that suspended it.
      |  1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(resumeEvalParamStep) -> """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>. Let _x_ be the value returned by the resumed computation.
      |  1. Let _x_ be _x_.""".stripMargin,
      resumeTopCtxtStep -> "Resume the context that is now on the top of the execution context stack as the running execution context.",
      noteStep -> "NOTE: At this point, it must be a numeric operation.",
      // -----------------------------------------------------------------------
      // special steps rarely used in the spec
      // -----------------------------------------------------------------------
      setFieldsWithIntrinsicsStep -> "set fields of _x_ with the values listed in <emu-xref href=\"#table-well-known-intrinsic-objects\"></emu-xref>. More description.",
      performBlockStep -> """perform the following substeps in an implementation-defined order, possibly interleaving parsing and error detection:
      |  1. Let _x_ be _x_.
      |  1. Set _x_ to _x_ + _x_.""".stripMargin,
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
      listCopyExpr -> "a List whose elements are the elements of _x_",
      recordEmptyExpr -> "Object { }",
      recordExpr -> "Object { [[Value]]: _x_ }",
      lengthExpr -> "the length of _x_",
      substrExpr -> "the substring of _x_ from _x_",
      substrExprTo -> "the substring of _x_ from _x_ to _x_",
      trim -> "the String value that is a copy of _x_ with both leading and trailing white space removed",
      trimStart -> "the String value that is a copy of _x_ with leading white space removed",
      trimEnd -> "the String value that is a copy of _x_ with trailing white space removed",
      numberOfExpr -> "the number of elements in _x_",
      numberOfBytesExpr -> "the number of bytes in _x_",
      numberOfListExpr -> "the number of elements in the List _x_",
      sourceTextExpr -> "the source text matched by |Identifier|",
      coveredByExpr -> "the |Identifier| that is covered by |Identifier|",
      getItemsExpr -> "the List of |Identifier| items in _x_, in source text order",
      intrExpr -> "%Array%",
      invokeAOExpr -> "ToObject(_x_ + _x_, -_x_)",
      invokeNumericExpr -> "Number::add(_x_, _x_)",
      invokeClosureExpr -> "_x_(_x_)",
      invokeMethodExpr -> "_x_.[[Value]](_x_ + _x_, -_x_)",
      invokeSDOExprZero -> "StringValue of |Identifier|",
      invokeSDOExprSingle -> ("StringValue of |Identifier| with argument |Identifier|"),
      invokeSDOExprMulti -> ("StringValue of |Identifier| with arguments |Identifier| and _x_"),
      invokeSDOExprEval -> "Evaluation of |Identifier|",
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
      ThisLiteral(false) -> "*this* value",
      ThisLiteral(true) -> "the *this* value",
      ThisParseNodeLiteral(None) -> "this Parse Node",
      NewTargetLiteral() -> "NewTarget",
      hex -> "0x0024",
      hexWithName -> "0x0024 (DOLLAR SIGN)",
      code -> "`|`",
      nt -> "|Identifier|",
      firstNt -> "first |Identifier|",
      firstNtWithArticle -> "the first |Identifier|",
      secondNt -> "second |Identifier|",
      secondNtWithArticle -> "the second |Identifier|",
      ntFlags -> "|A[~Yield, +Await]|",
      empty -> "~empty~",
      emptyStr -> """*""*""",
      str -> """*"abc"*""",
      strWithStar -> """*"abc\*"*""",
      strWithBasckSlash -> """*"abc\\"*""",
      fieldLit -> "[[Value]]",
      sym -> "%Symbol.iterator%",
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
      typeCheckCond -> "_x_ is a Base",
      notTypeCheckCond -> "_x_ is not a Base",
      eitherTypeCheckCond -> "_x_ is either a Base, a Base, or a Base",
      neitherTypeCheckCond -> "_x_ is neither a Base nor a Base",
      hasFieldCond -> "_x_ has a [[Value]] internal slot",
      hasMultipleFieldsCond -> "_x_ has [[Value]], [[Value]], and [[Value]] internal slots",
      noHasFieldCond -> "_x_ does not have a [[Value]] internal method",
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
      isCond -> "_x_ is the length of _x_",
      areCond -> "both _x_ and _x_ are not *true*",
      isEitherCond -> "_x_ is either *true* or *false*",
      isNeitherCond -> "_x_ is neither *true* nor *false*",
      binaryCondLt -> "_x_ < _x_ + _x_",
      inclusiveIntervalCondShort -> "2 ≤ _x_ ≤ 32",
      inclusiveIntervalCond -> "_x_ is in the inclusive interval from 2 to 32",
      notInclusiveIntervalCond -> "_x_ is not in the inclusive interval from 2 to 32",
      containsCond -> "_x_ contains _x_",
      notContainsCond -> "_x_ does not contain _x_",
      containsWhoseFieldCond -> "_x_ contains a Base whose [[Field]] is _x_",
      containsSuchThatCond -> "_x_ contains a Base _x_ such that _x_ is the length of _x_",
      compCond -> "_x_ and _x_",
      implyCond -> "If _x_ is the length of _x_, then _x_ is either *true* or *false*",
    )

    // -------------------------------------------------------------------------
    // algorithm references
    // -------------------------------------------------------------------------
    checkParseAndStringify("Reference", Reference)(
      x -> "_x_",
      xWithNt -> "|ArgumentList| _x_",
      access -> "_x_.[[Value]]",
      accessFieldDot -> "_x_.[[Value]]",
      accessCompDot -> "_x_.Value",
      accessFieldOf -> "the [[Value]] of _x_",
      accessCompOf -> "the Value component of _x_",
      accessFieldApo -> "_x_'s [[Value]] attribute",
      accessCompApo -> "_x_'s Value",
      valueOf -> "the value of _x_",
      intrField -> "_x_.[[%Array%]]",
      indexLookup -> "_x_[_x_]",
      bindingLookup -> "the binding for _x_ in _x_",
      ntLookup -> "the |Arguments| of _x_",
      firstElement -> "the first element of _x_",
      lastElement -> "the last element of _x_",
      intrObj -> "_x_'s intrinsic object named _x_",
      runningExecCtx -> "the running execution context",
      secondExecCtx ->
      "the second to top element of the execution context stack",
      currentRealmRec -> "the current Realm Record",
      activeFuncObj -> "the active function object",
      agentRec -> "the Agent Record of the surrounding agent",
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
      Type(EnumT("unused")) -> "~unused~",
      Type(EnumT("string", "symbol")) -> "~string~ or ~symbol~",
      Type(EnumT("start", "end", "start+end")) ->
      "~end~, ~start~, or ~start+end~",
      Type(ListT(NumberT)) -> "a List of Numbers",
      Type(ListT(AstT)) -> "a List of Parse Nodes",
      Type(
        ListT(StrT || UndefT),
      ) -> "a List of either Strings or *undefined*",
      Type(RecordT("EnvironmentRecord")) -> "an Environment Record",
      Type(NormalT(NumberT)) -> "a normal completion containing a Number",
      Type(NormalT(NumberT || BigIntT)) ->
      "a normal completion containing either a Number or a BigInt",
      Type(AbruptT) -> "an abrupt completion",
      Type(AbruptT("throw")) -> "a throw completion",
      Type(ReturnT || ThrowT) -> "a return completion or a throw completion",
      Type(NormalT(BigIntT) || AbruptT) ->
      "either a normal completion containing a BigInt or an abrupt completion",
      Type(NormalT(RecordT("PropertyDescriptor")) || AbruptT) ->
      "either a normal completion containing a Property Descriptor or an abrupt completion",
      // more complex
      Type(NumberT || BigIntT) -> "a Number or a BigInt",
      Type(NullT || ESValueT) -> "an ECMAScript language value",
      Type(ESValueT || AstT) ->
      "an ECMAScript language value or a Parse Node",
      Type(
        NormalT(ListT(StrT || NullT)) || AbruptT,
      ) -> "either a normal completion containing a List of either Strings or *null*, or an abrupt completion",
    )
  }

  init
}
