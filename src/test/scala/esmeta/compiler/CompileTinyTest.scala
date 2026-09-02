package esmeta.compiler

import esmeta.LINE_SEP
import esmeta.ir.{Func, FuncKind, IIf, Inst, Param => IRParam, Type => IRType}
import esmeta.ir.util.{Walker => IRWalker}
import esmeta.lang.*
import esmeta.lang.util.{
  Parser => LangParser,
  Parsers,
  UnitWalker => LangUnitWalker,
}
import esmeta.spec.*
import esmeta.spec.SyntaxDirectedOperationHead.Target
import org.jsoup.nodes.Element
import scala.collection.mutable.{Set => MSet}
import scala.compiletime.{constValue, erasedValue, summonFrom}
import scala.deriving.Mirror

/** compilation test for metalanguage snippets
  *
  * Each case compiles a single metalanguage step in a minimal algorithm and
  * compares the compiled IR with the IR parsed from the expected text. The
  * comparison is structural; see `normalizer` for the details.
  */
class CompileTinyTest extends CompilerTest {
  val name: String = "compilerCompileTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // assignment steps
    // -------------------------------------------------------------------------
    checkCompile("assignment steps")(
      "Let _x_ be _y_." -> "let x = y",
      // -----------------------------------------------------------------------
      "Let _x_ be a copy of _y_." -> "let x = (copy y)",
      // -----------------------------------------------------------------------
      "Set _x_ to _y_ + 1." -> "x = (+ y 1)",
      // -----------------------------------------------------------------------
      "Set _x_.[[Value]] to _y_." -> "x.Value = y",
      // -----------------------------------------------------------------------
      """Set _x_ as specified in <emu-xref href="#sec-foo"></emu-xref>.""" ->
      """x = clo<"Foo">""",
      // -----------------------------------------------------------------------
      "Set the code evaluation state of _x_ such that when evaluation " +
      "is resumed for that execution context, _y_ will be called with " +
      "no arguments." ->
      """x.__RESUME_CONT__ = cont<"Test:cont0">""",
      // -----------------------------------------------------------------------
      "Set fields of _x_ with the values listed in " +
      """<emu-xref href="#table-well-known-intrinsic-objects"></emu-xref>. """ +
      "More description." ->
      "x = @INTRINSICS",
    )

    // -------------------------------------------------------------------------
    // invocation steps
    // -------------------------------------------------------------------------
    checkCompile("invocation steps")(
      "Perform ToObject(_x_)." -> """call %0 = clo<"ToObject">(x)""",
      // -----------------------------------------------------------------------
      "Perform ! ToObject(_x_)." ->
      """call %0 = clo<"ToObject">(x)
      |assert (? %0: Normal)
      |%0 = %0.Value""".stripMargin,
      // -----------------------------------------------------------------------
      "IfAbruptCloseIterator(_x_, _y_)." ->
      """if (&& (? x: Completion) (! (= x.Type ~normal~))) {
      |  return y
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // list mutation steps
    // -------------------------------------------------------------------------
    checkCompile("list mutation steps")(
      "Append _x_ to _y_." -> "push y < x",
      // -----------------------------------------------------------------------
      "Prepend _x_ to _y_." -> "push x > y",
      // -----------------------------------------------------------------------
      "Insert _x_ as the first element of _y_." -> "push x > y",
      // -----------------------------------------------------------------------
      "Add _x_ to _y_." -> "push y < x",
      // -----------------------------------------------------------------------
      "Remove _x_ from _y_." -> """call %0 = clo<"__REMOVE_ELEM__">(x, y)""",
      // -----------------------------------------------------------------------
      "Remove the first element of _x_." -> "pop %0 < x",
      // -----------------------------------------------------------------------
      "Remove the last element of _x_." -> "pop x > %0",
      // -----------------------------------------------------------------------
      "Remove the first _y_ elements from _x_." ->
      """%0 = 0
      |%1 = y
      |while (< %0 %1) {
      |  pop %2 < x
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "Remove the last _y_ elements from _x_." ->
      """%0 = 0
      |%1 = y
      |while (< %0 %1) {
      |  pop x > %2
      |  %0 = (+ %0 1)
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // execution context steps
    // -------------------------------------------------------------------------
    checkCompile("execution context steps")(
      "Push _x_ onto the execution context stack; _x_ is now the " +
      "running execution context." ->
      "push x > @EXECUTION_STACK",
      // -----------------------------------------------------------------------
      "Suspend the running execution context." -> "nop",
      // -----------------------------------------------------------------------
      "Suspend _x_." -> "nop",
      // -----------------------------------------------------------------------
      "Suspend _x_ and remove it from the execution context stack." ->
      "pop %0 < @EXECUTION_STACK",
      // -----------------------------------------------------------------------
      "Remove _x_ from the execution context stack." ->
      "pop %0 < @EXECUTION_STACK",
      // -----------------------------------------------------------------------
      "Remove _x_ from the execution context stack and restore _y_ as " +
      "the running execution context." ->
      "pop %0 < @EXECUTION_STACK",
      // -----------------------------------------------------------------------
      "Resume the context that is now on the top of the execution " +
      "context stack as the running execution context." ->
      "nop",
    )

    // -------------------------------------------------------------------------
    // conditional steps
    // -------------------------------------------------------------------------
    checkCompile("conditional steps")(
      "Assert: _x_ is a String." -> "assert (? x: String)",
      // -----------------------------------------------------------------------
      "Assert: _x_ is *true* and Foo(_y_) is *true*." ->
      """%0 = (= x true)
      |if %0 {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |}
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_, let _y_ be _x_." ->
      """if x {
      |  let y = x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_, let _y_ be _x_. Else, let _y_ be *undefined*." ->
      """if x {
      |  let y = x
      |} else {
      |  let y = undefined
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. If _x_, then
      |    1. Let _y_ be _x_.
      |  1. Else,
      |    1. Let _y_ be *undefined*.""".stripMargin ->
      """if x {
      |  let y = x
      |} else {
      |  let y = undefined
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is *true* and Foo(_y_) is *true*, throw a *TypeError* " +
      "exception." ->
      """%0 = (= x true)
      |if %0 {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |}
      |if %0 {
      |  call %2 = clo<"__NEW_ERROR_OBJ__">("%TypeError.prototype%")
      |  call %3 = clo<"ThrowCompletion">(%2)
      |  return %3
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is *true* or _y_ is *true*, return *false*." ->
      """if (|| (= x true) (= y true)) {
      |  return false
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is *true* and Foo(_y_) is *true*, let _z_ be *true*. " +
      "Else, let _z_ be *false*." ->
      """%0 = (= x true)
      |if %0 {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |}
      |if %0 {
      |  let z = true
      |} else {
      |  let z = false
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // loop steps
    // -------------------------------------------------------------------------
    checkCompile("loop steps")(
      "Repeat, let _x_ be _y_." ->
      """while true {
      |  let x = y
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "Repeat, while _x_ is *true*, set _y_ to _y_ + 1." ->
      """while (= x true) {
      |  y = (+ y 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "Repeat, until _x_ is *true*, set _y_ to _y_ + 1." ->
      """while (! (= x true)) {
      |  y = (+ y 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each element _x_ of _y_, append _x_ to _z_." ->
      """%1 = y
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  push z < x
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each String _x_ of _y_, append _x_ to _z_." ->
      """%1 = y
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  if (? x: String) {
      |    push z < x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each element _x_ of _y_, in reverse List order, append _x_ " +
      "to _z_." ->
      """%1 = y
      |%0 = (- (sizeof %1) 1)
      |while (! (< %0 0)) {
      |  let x = %1[%0]
      |  push z < x
      |  %0 = (- %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each integer _x_ such that 0 ≤ _x_ ≤ 5, in ascending order, " +
      "append _x_ to _z_." ->
      """let x = 0
      |%0 = 5
      |while (! (< %0 x)) {
      |  push z < x
      |  x = (+ x 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each integer _x_ such that 0 " +
      "< _x_ < 5, in descending order, append _x_ to _z_." ->
      """let x = (+ 5 1)
      |%0 = 0
      |while (< %0 x) {
      |  push z < x
      |  x = (- x 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each own property key _x_ of _y_ such that _x_ is an array " +
      "index, in ascending numeric index order, append _x_ to _z_." ->
      """%1 = (keys-int y.__MAP__)
      |%0 = 0
      |while (< %0 (sizeof %1)) {
      |  let x = %1[%0]
      |  call %2 = clo<"__IS_ARRAY_INDEX__">(x)
      |  if %2 {
      |    push z < x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each own property key _x_ of _y_ such that _x_ is a String, " +
      "in descending chronological order of property creation, append " +
      "_x_ to _z_." ->
      """%1 = (keys y.__MAP__)
      |%0 = (sizeof %1)
      |while (< 0 %0) {
      |  %0 = (- %0 1)
      |  let x = %1[%0]
      |  if (? x: String) {
      |    push z < x
      |  }
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "For each child node _x_ of _y_, do append _x_ to _z_." ->
      """%1 = y
      |%0 = 0
      |%2 = (sizeof %1)
      |while (< %0 %2) {
      |  if (exists %1[%0]) {
      |    let x = %1[%0]
      |    push z < x
      |  }
      |  %0 = (+ %0 1)
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // return steps
    // -------------------------------------------------------------------------
    checkCompile("return steps")(
      "Return _x_." -> "return x",
      // -----------------------------------------------------------------------
      "Return ? Foo(_x_)." ->
      """call %0 = clo<"Foo">(x)
      |assert (? %0: Completion)
      |if (? %0: Abrupt) return %0
      |else %0 = %0.Value
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return ! Foo(_x_)." ->
      """call %0 = clo<"Foo">(x)
      |assert (? %0: Normal)
      |%0 = %0.Value
      |return %0""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // return steps of completion-returning algorithms
    // -------------------------------------------------------------------------
    checkCompile(
      "return steps of completion-returning algorithms",
      needRetComp = true,
    )(
      "Return _x_." ->
      """if (? x: Completion) return x
      |call %0 = clo<"NormalCompletion">(x)
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return *undefined*." ->
      """call %0 = clo<"NormalCompletion">(undefined)
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return Foo(_x_)." ->
      """call %0 = clo<"Foo">(x)
      |if (? %0: Completion) return %0
      |call %1 = clo<"NormalCompletion">(%0)
      |return %1""".stripMargin,
      // -----------------------------------------------------------------------
      "Return ? Foo(_x_)." ->
      """call %0 = clo<"Foo">(x)
      |assert (? %0: Completion)
      |if (? %0: Abrupt) return %0
      |else return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return ! Foo(_x_)." ->
      """call %0 = clo<"Foo">(x)
      |assert (? %0: Normal)
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return NormalCompletion(_x_)." ->
      """call %0 = clo<"NormalCompletion">(x)
      |if (? %0: Completion) return %0
      |call %1 = clo<"NormalCompletion">(%0)
      |return %1""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // other steps
    // -------------------------------------------------------------------------
    checkCompile("other steps")(
      "Throw a *TypeError* exception." ->
      """call %0 = clo<"__NEW_ERROR_OBJ__">("%TypeError.prototype%")
      |call %1 = clo<"ThrowCompletion">(%0)
      |return %1""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. Resume _x_ passing _y_. If _x_ is ever resumed again, let _z_ be the Completion Record with which it is resumed.
      |  1. Return _z_.""".stripMargin ->
      """x.__RESUME_CONT__ = cont<"Test:cont0">
      |pop %0 < x.__RETURN_CONT__
      |call %1 = %0(y)""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta> using _y_ as the result of the operation that suspended it.
      |  1. Return _x_.""".stripMargin ->
      """if (! (exists x.__RETURN_CONT__)) x.__RETURN_CONT__ = (list [])
      |push cont<"Test:cont0"> > x.__RETURN_CONT__
      |call %0 = x.__RESUME_CONT__(y)""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>. Let _y_ be the value returned by the resumed computation.
      |  1. Return _y_.""".stripMargin ->
      """if (! (exists x.__RETURN_CONT__)) x.__RETURN_CONT__ = (list [])
      |push cont<"Test:cont0"> > x.__RETURN_CONT__
      |call %0 = x.__RESUME_CONT__()""".stripMargin,
      // -----------------------------------------------------------------------
      "NOTE: This step is just a note." -> "nop",
      // -----------------------------------------------------------------------
      """
      |  1. Let _x_ be _y_.
      |  1. Return _x_.""".stripMargin ->
      """let x = y
      |return x""".stripMargin,
      // -----------------------------------------------------------------------
      """Perform the following substeps in an implementation-defined order, possibly interleaving parsing and error detection:
      |  1. Let _x_ be _y_.""".stripMargin ->
      "let x = y",
      // -----------------------------------------------------------------------
      "Do something that is not yet supported." ->
      """(yet "Do something that is not yet supported.")""",
      // -----------------------------------------------------------------------
      "Let _x_ be something not yet supported." ->
      """(yet "Let _x_ be something not yet supported.")""",
    )

    // -------------------------------------------------------------------------
    // manual compile rules
    // -------------------------------------------------------------------------
    checkCompile("manual compile rules")(
      "Change its bound value to _V_." ->
      "envRec.__MAP__[N].__BOUND_VALUE__ = V",
      // -----------------------------------------------------------------------
      "Assert: The execution context stack is not empty." ->
      "assert (! (= (sizeof @EXECUTION_STACK) 0))",
    )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
    checkCompile("expressions")(
      "Let _x_ be the string-concatenation of _y_ and _z_." ->
      "let x = (concat y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the list-concatenation of _y_ and _z_." ->
      """call %0 = clo<"__FLAT_LIST__">((list [y, z]))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be a List whose elements are the elements of _y_." ->
      "let x = (copy y)",
      // -----------------------------------------------------------------------
      "Let _x_ be a copy of the List _y_." -> "let x = (copy y)",
      // -----------------------------------------------------------------------
      "Let _x_ be a copy of the running execution context." ->
      "let x = (copy @EXECUTION_STACK[0])",
      // -----------------------------------------------------------------------
      "Let _x_ be Object { }." ->
      """let x = (record [Object] {
      |  "__MAP__" : (map[Record[Symbol] | String, Record[PropertyDescriptor]]),
      |  "PrivateElements" : (list []),
      |})""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be Object { [[Value]]: _y_ }." ->
      """let x = (record [Object] {
      |  "Value" : y,
      |  "__MAP__" : (map[Record[Symbol] | String, Record[PropertyDescriptor]]),
      |  "PrivateElements" : (list []),
      |})""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be PropertyDescriptor { [[Value]]: _y_ }." ->
      """let x = (record [PropertyDescriptor] {
      |  "Value" : y,
      |})""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be the length of _y_." -> "let x = (sizeof y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the length of the string-concatenation of _y_ and _z_." ->
      """%0 = (concat y z)
      |let x = (sizeof %0)""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be the substring of _y_ from _z_." -> "let x = (substring y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the substring of _y_ from _z_ to _w_." ->
      "let x = (substring y z w)",
      // -----------------------------------------------------------------------
      "Let _x_ be the String value that is a copy of _y_ with both " +
      "leading and trailing white space removed." ->
      "let x = (trim (trim > y) <)",
      // -----------------------------------------------------------------------
      "Let _x_ be the String value that is a copy of _y_ with leading " +
      "white space removed." ->
      "let x = (trim > y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the number of elements in _y_." -> "let x = (sizeof y)",
      // -----------------------------------------------------------------------
      "Let _x_ be %Array%." ->
      """let x = @EXECUTION_STACK[0].Realm.Intrinsics["%Array%"]""",
      // -----------------------------------------------------------------------
      "Let _x_ be the source text matched by |Identifier|." ->
      "let x = (source-text (grammar-symbol |Identifier|))",
      // -----------------------------------------------------------------------
      "Let _x_ be the |Identifier| that is covered by |Identifier|." ->
      "let x = (parse (grammar-symbol |Identifier|) (grammar-symbol |Identifier|))",
      // -----------------------------------------------------------------------
      "Let _x_ be the List of |Identifier| items in _y_, in source text " +
      "order." ->
      """let x = (yet "the List of |Identifier| items in _y_, in source text order")""",
      // -----------------------------------------------------------------------
      "Let _x_ be ToObject(_y_, _z_)." ->
      """call %0 = clo<"ToObject">(y, z)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be Number::add(_y_, _z_)." ->
      """call %0 = clo<"Number::add">(y, z)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be _y_(_z_)." ->
      """call %0 = y(z)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be _y_.[[Value]](_z_)." ->
      """call %0 = y.Value(y, z)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be StringValue of |Identifier|." ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue()
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be StringValue of |Identifier| with argument _y_." ->
      """sdo-call %0 = (grammar-symbol |Identifier|)->StringValue(y)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be ? ToObject(_y_)." ->
      """call %0 = clo<"ToObject">(y)
      |assert (? %0: Completion)
      |if (? %0: Abrupt) return %0
      |else %0 = %0.Value
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be ! ToObject(_y_)." ->
      """call %0 = clo<"ToObject">(y)
      |assert (? %0: Normal)
      |%0 = %0.Value
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be « »." -> "let x = (list [])",
      // -----------------------------------------------------------------------
      "Let _x_ be « _y_, _z_ »." -> "let x = (list [y, z])",
      // -----------------------------------------------------------------------
      "Let _x_ be a new empty List." -> "let x = (list [])",
      // -----------------------------------------------------------------------
      "Let _x_ be a List whose sole element is _y_." -> "let x = (list [y])",
      // -----------------------------------------------------------------------
      "Let _x_ be a List of the integers in the interval from 0 " +
      "(inclusive) to _y_ (exclusive), in ascending order." ->
      """%0 = 0
      |%1 = y
      |%2 = %0
      |%3 = (list [])
      |while (< %2 %1) {
      |  push %3 < %2
      |  %2 = (+ %2 1)
      |}
      |let x = %3""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be the definition specified in " +
      """<emu-xref href="#sec-foo"></emu-xref>.""" ->
      """let x = clo<"Foo">""",
      // -----------------------------------------------------------------------
      "Let _x_ be the number of non-optional parameters of the function " +
      """definition in <emu-xref href="#sec-foo"></emu-xref>.""" ->
      "let x = 1",
      // -----------------------------------------------------------------------
      "Let _x_ be the internal slots listed in " +
      """<emu-xref href="#table-x"></emu-xref>.""" ->
      """let x = (list ["Value"])""",
      // -----------------------------------------------------------------------
      "Let _x_ be the sole element of « _y_ »." ->
      """%0 = (list [y])
      |let x = %0[0]""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be the code unit at index _z_ within _y_." -> "let x = y[z]",
      // -----------------------------------------------------------------------
      """Let _x_ be a new Abstract Closure with parameters (_a_, _b_) that captures _y_ and performs the following steps when called:
      |  1. Return _a_.""".stripMargin ->
      """let x = clo<"Test:clo0", [y]>""",
    )

    // -------------------------------------------------------------------------
    // calculation expressions
    // -------------------------------------------------------------------------
    checkCompile("calculation expressions")(
      "Let _x_ be _y_ + _z_." -> "let x = (+ y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ - _z_." -> "let x = (- y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ × _z_." -> "let x = (* y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ / _z_." -> "let x = (/ y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ modulo _z_." -> "let x = (% y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_<sup>_z_</sup>." -> "let x = (** y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be -_y_." -> "let x = (- y)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ × (_z_ + _w_)." -> "let x = (* y (+ z w))",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ plus _z_." -> "let x = (+ y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_ times _z_." -> "let x = (* y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be ℝ(_y_)." -> "let x = ([math] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be 𝔽(_y_)." -> "let x = ([number] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be ℤ(_y_)." -> "let x = ([bigInt] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be an implementation-approximated Number value " +
      "representing _y_." ->
      "let x = ([approx-number] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the Number value of the code unit at index _z_ within " +
      "_y_." ->
      "let x = ([number] y[z])",
      // -----------------------------------------------------------------------
      "Let _x_ be the BigInt value of the code unit at index _z_ within " +
      "_y_." ->
      "let x = ([bigInt] y[z])",
      // -----------------------------------------------------------------------
      "Let _x_ be the numeric value of the code unit at index _z_ " +
      "within _y_." ->
      "let x = ([math] y[z])",
      // -----------------------------------------------------------------------
      "Let _x_ be the result of clamping _y_ between 0 and _z_." ->
      """call %0 = clo<"__CLAMP__">(y, 0, z)
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be the result of applying the bitwise AND operation to " +
      "_y_ and _z_." ->
      "let x = (& y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the result of applying the bitwise exclusive OR (XOR) " +
      "operation to _y_ and _z_." ->
      "let x = (^ y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the result of applying the bitwise inclusive OR " +
      "operation to _y_ and _z_." ->
      "let x = (| y z)",
    )

    // -------------------------------------------------------------------------
    // mathematical operation expressions
    // -------------------------------------------------------------------------
    checkCompile("mathematical operation expressions")(
      "Let _x_ be the negation of _y_." -> "let x = (- y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the sum of _y_ and _z_." -> "let x = (+ y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the product of _y_ and _z_." -> "let x = (* y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the difference _y_ minus _z_." -> "let x = (- y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the raising _y_ to the _z_ power." -> "let x = (** y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the subtracting 1 from the exponential function of _y_." ->
      "let x = ([math:expm1] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the cosine of _y_." -> "let x = ([math:cos] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the cube root of _y_." -> "let x = ([math:cbrt] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the exponential function of _y_." -> "let x = ([math:exp] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the hyperbolic cosine of _y_." -> "let x = ([math:cosh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the hyperbolic sine of _y_." -> "let x = ([math:sinh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the hyperbolic tangent of _y_." -> "let x = ([math:tanh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse cosine of _y_." -> "let x = ([math:acos] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse hyperbolic cosine of _y_." ->
      "let x = ([math:acosh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse hyperbolic sine of _y_." ->
      "let x = ([math:asinh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse hyperbolic tangent of _y_." ->
      "let x = ([math:atanh] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse sine of _y_." -> "let x = ([math:asin] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse tangent of the quotient _y_ / _z_." ->
      "let x = ([math:atan2] y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be the inverse tangent of _y_." -> "let x = ([math:atan] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the sine of _y_." -> "let x = ([math:sin] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the square root of _y_." -> "let x = ([math:sqrt] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be the tangent of _y_." -> "let x = ([math:tan] y)",
    )

    // -------------------------------------------------------------------------
    // mathematical function expressions
    // -------------------------------------------------------------------------
    checkCompile("mathematical function expressions")(
      "Let _x_ be max(_y_, _z_)." -> "let x = (max y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be min(_y_, _z_)." -> "let x = (min y z)",
      // -----------------------------------------------------------------------
      "Let _x_ be abs(_y_)." -> "let x = (abs y)",
      // -----------------------------------------------------------------------
      "Let _x_ be floor(_y_)." -> "let x = (floor y)",
      // -----------------------------------------------------------------------
      "Let _x_ be log10(_y_)." -> "let x = ([math:log10] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be log2(_y_)." -> "let x = ([math:log2] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be ln(_y_)." -> "let x = ([math:log] y)",
      // -----------------------------------------------------------------------
      "Let _x_ be truncate(_y_)." ->
      """%0 = y
      |if (< %0 0) %0 = (- (floor (- %0)))
      |else %0 = (floor %0)
      |let x = %0""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // literals
    // -------------------------------------------------------------------------
    checkCompile("literals")(
      "Let _x_ be the *this* value." -> "let x = this",
      // -----------------------------------------------------------------------
      "Let _x_ be this Parse Node." -> "let x = this",
      // -----------------------------------------------------------------------
      "Let _x_ be NewTarget." -> "let x = NewTarget",
      // -----------------------------------------------------------------------
      "Let _x_ be 0x0024." -> "let x = 36",
      // -----------------------------------------------------------------------
      "Let _x_ be 0x0024 (DOLLAR SIGN)." -> "let x = 36cu",
      // -----------------------------------------------------------------------
      "Let _x_ be `|`." -> """let x = "|"""",
      // -----------------------------------------------------------------------
      "Let _x_ be |Identifier|." -> "let x = (grammar-symbol |Identifier|)",
      // -----------------------------------------------------------------------
      "Let _x_ be the first |Identifier|." ->
      "let x = (grammar-symbol |Identifier|)",
      // -----------------------------------------------------------------------
      "Let _x_ be ~empty~." -> "let x = ~empty~",
      // -----------------------------------------------------------------------
      """Let _x_ be *""*.""" -> """let x = """"",
      // -----------------------------------------------------------------------
      """Let _x_ be *"abc"*.""" -> """let x = "abc"""",
      // -----------------------------------------------------------------------
      "Let _x_ be %Symbol.iterator%." -> "let x = @SYMBOL.iterator",
      // -----------------------------------------------------------------------
      "Let _x_ be a newly created *TypeError* object." ->
      """call %0 = clo<"__NEW_ERROR_OBJ__">("%TypeError.prototype%")
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be +∞." -> "let x = +INF",
      // -----------------------------------------------------------------------
      "Let _x_ be -∞." -> "let x = -INF",
      // -----------------------------------------------------------------------
      "Let _x_ be 0.5." -> "let x = 0.5",
      // -----------------------------------------------------------------------
      "Let _x_ be π." -> "let x = @MATH_PI",
      // -----------------------------------------------------------------------
      "Let _x_ be 2π." -> "let x = (* 2 @MATH_PI)",
      // -----------------------------------------------------------------------
      "Let _x_ be *+0*<sub>𝔽</sub>." -> "let x = 0.0f",
      // -----------------------------------------------------------------------
      "Let _x_ be *-0*<sub>𝔽</sub>." -> "let x = -0.0f",
      // -----------------------------------------------------------------------
      "Let _x_ be *+∞*<sub>𝔽</sub>." -> "let x = +NUM_INF",
      // -----------------------------------------------------------------------
      "Let _x_ be *NaN*." -> "let x = NaN",
      // -----------------------------------------------------------------------
      "Let _x_ be *1*<sub>𝔽</sub>." -> "let x = 1.0f",
      // -----------------------------------------------------------------------
      "Let _x_ be *1*<sub>ℤ</sub>." -> "let x = 1n",
      // -----------------------------------------------------------------------
      "Let _x_ be *true*." -> "let x = true",
      // -----------------------------------------------------------------------
      "Let _x_ be *false*." -> "let x = false",
      // -----------------------------------------------------------------------
      "Let _x_ be *undefined*." -> "let x = undefined",
      // -----------------------------------------------------------------------
      "Let _x_ be *null*." -> "let x = null",
      // -----------------------------------------------------------------------
      "Set _x_ to Undefined." -> "x = @Undefined",
      // -----------------------------------------------------------------------
      "Set _x_ to Null." -> "x = @Null",
      // -----------------------------------------------------------------------
      "Set _x_ to Boolean." -> "x = @Boolean",
      // -----------------------------------------------------------------------
      "Set _x_ to String." -> "x = @String",
      // -----------------------------------------------------------------------
      "Set _x_ to Symbol." -> "x = @Symbol",
      // -----------------------------------------------------------------------
      "Set _x_ to Number." -> "x = @Number",
      // -----------------------------------------------------------------------
      "Set _x_ to BigInt." -> "x = @BigInt",
      // -----------------------------------------------------------------------
      "Set _x_ to Object." -> "x = @Object",
      // -----------------------------------------------------------------------
      "Let _x_ be the grammar symbol |Identifier|." ->
      "let x = (grammar-symbol |Identifier|)",
      // -----------------------------------------------------------------------
      "Let _x_ be an instance of <emu-grammar>Identifier : " +
      "IdentifierName</emu-grammar>." ->
      "let x = |Identifier|<0>",
      // -----------------------------------------------------------------------
      "Let _x_ be msPerDay." -> "let x = 86400000",
      // -----------------------------------------------------------------------
      "Let _x_ be the String value _y_." -> "let x = y",
    )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    checkCompile("references")(
      "Let _x_ be |ArgumentList| _y_." -> "let x = y",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_.[[Value]]." -> "let x = y.Value",
      // -----------------------------------------------------------------------
      "Let _x_ be the [[Value]] of _y_." -> "let x = y.Value",
      // -----------------------------------------------------------------------
      "Let _x_ be the Value component of _y_." -> "let x = y.Value",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_'s [[Value]] attribute." -> "let x = y.Value",
      // -----------------------------------------------------------------------
      "Let _x_ be the value of _y_." -> "let x = y",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_.[[%Array%]]." -> """let x = y["%Array%"]""",
      // -----------------------------------------------------------------------
      "Let _x_ be _y_[_z_]." -> "let x = y[z]",
      // -----------------------------------------------------------------------
      "Let _x_ be the binding for _y_ in _z_." -> "let x = z.__MAP__[y]",
      // -----------------------------------------------------------------------
      "Let _x_ be the |Arguments| of _y_." -> "let x = y.Arguments",
      // -----------------------------------------------------------------------
      "Let _x_ be the first element of _y_." -> "let x = y[0]",
      // -----------------------------------------------------------------------
      "Let _x_ be the last element of _y_." ->
      """%0 = y
      |let x = %0[(- (sizeof %0) 1)]""".stripMargin,
      // -----------------------------------------------------------------------
      "Let _x_ be _y_'s intrinsic object named _z_." ->
      "let x = y.Intrinsics[z]",
      // -----------------------------------------------------------------------
      "Let _x_ be the running execution context." ->
      "let x = @EXECUTION_STACK[0]",
      // -----------------------------------------------------------------------
      "Let _x_ be the second to top element of the execution context stack." ->
      "let x = @EXECUTION_STACK[1]",
      // -----------------------------------------------------------------------
      "Let _x_ be the current Realm Record." ->
      "let x = @EXECUTION_STACK[0].Realm",
      // -----------------------------------------------------------------------
      "Let _x_ be the active function object." ->
      "let x = @EXECUTION_STACK[0].Function",
      // -----------------------------------------------------------------------
      "Let _x_ be the Agent Record of the surrounding agent." ->
      "let x = @AGENT_RECORD",
    )

    // -------------------------------------------------------------------------
    // conditions
    // -------------------------------------------------------------------------
    checkCompile("conditions")(
      "If _x_, return *true*." ->
      """if x {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is a String, return *true*." ->
      """if (? x: String) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is not a String, return *true*." ->
      """if (! (? x: String)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is either a String or a Number, return *true*." ->
      """if (|| (? x: String) (? x: Number)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is neither a String nor a Number, return *true*." ->
      """if (! (|| (? x: String) (? x: Number))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ has a [[Value]] internal slot, return *true*." ->
      """if (exists x.Value) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ has [[Value]], [[Writable]], and [[Get]] internal slots, " +
      "return *true*." ->
      """if (&& (&& (exists x.Value) (exists x.Writable)) (exists x.Get)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ does not have a [[Value]] internal method, return *true*." ->
      """if (! (exists x.Value)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ has a binding for _y_, return *true*." ->
      """if (exists x.__MAP__[y]) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ does not have a binding for _y_, return *true*." ->
      """if (! (exists x.__MAP__[y])) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If |Identifier| is <emu-grammar>Identifier : " +
      "Identifier</emu-grammar>, return *true*." ->
      """if (yet "|Identifier| is <emu-grammar>Identifier : Identifier</emu-grammar>") {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is finite, return *true*." ->
      """if (|| (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Math | BigInt)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is a finite Number, return *true*." ->
      """if (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ and _y_ are finite Numbers, return *true*." ->
      """if (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (&& (? y: Number) (! (? y: Number[-INF, +INF, NaN])))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is a non-zero finite Number, return *true*." ->
      """if (&& (&& (? x: Number) (! (? x: Number[-INF, +INF, NaN]))) (? x: Number[NonZero, -INF, +INF, NaN])) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is an abrupt completion, return *true*." ->
      """if (&& (? x: Completion) (! (= x.Type ~normal~))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is a normal completion, return *true*." ->
      """if (&& (? x: Completion) (= x.Type ~normal~)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is duplicate entries, return *true*." ->
      """call %0 = clo<"__HAS_DUPLICATE__">(x)
      |if %0 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is present, return *true*." ->
      """if (exists x) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is empty, return *true*." ->
      """if (= (sizeof x) 0) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is strict mode code, return *true*." ->
      """if true {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is an array index, return *true*." ->
      """call %0 = clo<"__IS_ARRAY_INDEX__">(x)
      |if %0 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is the length of _y_, return *true*." ->
      """if (= x (sizeof y)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If both _x_ and _y_ are not *true*, return *true*." ->
      """if (&& (! (= x true)) (! (= y true))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is either *true* or *false*, return *true*." ->
      """if (|| (= x true) (= x false)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is neither *true* nor *false*, return *true*." ->
      """if (! (|| (= x true) (= x false))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ < _y_ + 1, return *true*." ->
      """if (< x (+ y 1)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ = _y_, return *true*." ->
      """if (== x y) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ ≠ _y_, return *true*." ->
      """if (! (== x y)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ ≥ _y_, return *true*." ->
      """if (! (< x y)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If 2 ≤ _x_ ≤ 32, return *true*." ->
      """if (! (|| (< x 2) (< 32 x))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is in the inclusive interval from 2 to 32, return *true*." ->
      """if (! (|| (< x 2) (< 32 x))) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is not in the inclusive interval from 2 to 32, return *true*." ->
      """if (|| (< x 2) (< 32 x)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ contains _y_, return *true*." ->
      """if (contains x y) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ does not contain _y_, return *true*." ->
      """if (! (contains x y)) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ contains a Record whose [[Value]] is _y_, return *true*." ->
      """%1 = x
      |%2 = 0
      |%3 = false
      |while (&& (! %3) (< %2 (sizeof %1))) {
      |  %0 = %1[%2]
      |  %3 = (&& (? %0: Record) (= %0.Value y))
      |  %2 = (+ %2 1)
      |}
      |if %3 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ contains a Record _r_ such that _r_.[[Value]] is _y_, " +
      "return *true*." ->
      """%0 = x
      |%1 = 0
      |%2 = false
      |while (&& (! %2) (< %1 (sizeof %0))) {
      |  let r = %0[%1]
      |  %2 = (&& (? r: Record) (= r.Value y))
      |  %1 = (+ %1 1)
      |}
      |if %2 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ and _y_, return *true*." ->
      """if (&& x y) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ or _y_, return *true*." ->
      """if (|| x y) {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "Assert: If _x_, then _y_." -> "assert (|| (! x) y)",
      // -----------------------------------------------------------------------
      "Assert: If _x_ is *true*, then Foo(_y_) is *true*." ->
      """%0 = (= x true)
      |if %0 {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |} else %0 = true
      |assert %0""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is *true* and Foo(_y_) is *true*, return *true*." ->
      """%0 = (= x true)
      |if %0 {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |}
      |if %0 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If _x_ is *true* or Foo(_y_) is *true*, return *true*." ->
      """%0 = (= x true)
      |if %0 {} else {
      |  call %1 = clo<"Foo">(y)
      |  %0 = (= %1 true)
      |}
      |if %0 {
      |  return true
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      "If Foo(_x_) is *true* and Foo(_y_) is *true*, return *true*." ->
      """call %1 = clo<"Foo">(x)
      |%0 = (= %1 true)
      |if %0 {
      |  call %2 = clo<"Foo">(y)
      |  %0 = (= %2 true)
      |}
      |if %0 {
      |  return true
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // syntax-directed operations
    // -------------------------------------------------------------------------
    checkCompile("syntax-directed operations", sdo = true)(
      "Let _x_ be |IdentifierName|." -> "let x = this[0]",
      // -----------------------------------------------------------------------
      "Let _x_ be the source text matched by |IdentifierName|." ->
      "let x = (source-text this[0])",
      // -----------------------------------------------------------------------
      "Let _x_ be the |Identifier| that is covered by |IdentifierName|." ->
      "let x = (parse this[0] this)",
      // -----------------------------------------------------------------------
      "Let _x_ be the List of |IdentifierName| items in " +
      "|IdentifierName|, in source text order." ->
      """call %0 = clo<"__GET_ITEMS__">(this[0], this[0], (grammar-symbol |IdentifierName|))
      |let x = %0""".stripMargin,
      // -----------------------------------------------------------------------
      "Return StringValue of |IdentifierName|." ->
      """sdo-call %0 = this[0]->StringValue()
      |return %0""".stripMargin,
      // -----------------------------------------------------------------------
      "If |Identifier| is <emu-grammar>Identifier : " +
      "IdentifierName</emu-grammar>, return *true*." ->
      """if (? this: Ast[Identifier[0]]) {
      |  return true
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // auxiliary functions (abstract closures and continuations)
    // -------------------------------------------------------------------------
    checkCompileFuncs("auxiliary functions")(
      "Set the code evaluation state of _x_ such that when evaluation " +
      "is resumed for that execution context, _y_ will be called with " +
      "no arguments." ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  call %0 = y()
      |  pop %1 < x.__RETURN_CONT__
      |  call %2 = %1(%0)
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. Resume _x_ passing _y_. If _x_ is ever resumed again, let _z_ be the Completion Record with which it is resumed.
      |  1. Return _z_.""".stripMargin ->
      """def <CONT>:Test:cont0(
      |  z: Unknown,
      |): Unknown = {
      |  return z
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta> using _y_ as the result of the operation that suspended it.
      |  1. Return _x_.""".stripMargin ->
      """def <CONT>:Test:cont0(
      |): Unknown = {
      |  return x
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      """
      |  1. <emu-meta effects="user-code">Resume the suspended evaluation of _x_</emu-meta>. Let _y_ be the value returned by the resumed computation.
      |  1. Return _y_.""".stripMargin ->
      """def <CONT>:Test:cont0(
      |  y: Unknown,
      |): Unknown = {
      |  return y
      |}""".stripMargin,
      // -----------------------------------------------------------------------
      """Let _x_ be a new Abstract Closure with parameters (_a_, _b_) that captures _y_ and performs the following steps when called:
      |  1. Return _a_.""".stripMargin ->
      """def <CLO>:Test:clo0(
      |  a: Unknown,
      |  b: Unknown,
      |): Unknown = {
      |  if (? a: Completion) return a
      |  call %0 = clo<"NormalCompletion">(a)
      |  return %0
      |}""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // coverage of the metalanguage syntax
    // -------------------------------------------------------------------------
    checkCoverage
  }

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------
  /** an abstract operation of the fixture specification */
  private lazy val fooAlgo: Algorithm =
    val head = AbstractOperationHead(
      false,
      "Foo",
      List(Param("x", UnknownType)),
      UnknownType,
    )
    val algo = Algorithm(head, Step.from("return _x_."), "")
    algo.elem = Element("emu-alg").attr("id", "sec-foo")
    algo

  /** a shorthand of the fixture specification */
  private lazy val shorthandAlgo: Algorithm = Algorithm(
    AbstractOperationHead(
      false,
      "IfAbruptCloseIterator",
      List(Param("value", UnknownType), Param("iteratorRecord", UnknownType)),
      UnknownType,
    ),
    Step.from("if _value_ is an abrupt completion, return _iteratorRecord_."),
    "",
  )

  /** a fixture specification with the minimum for the cases above */
  private lazy val spec: Spec = Spec(
    grammar = Grammar(List(Production.from("Identifier :\n  IdentifierName"))),
    algorithms = List(fooAlgo, shorthandAlgo),
    constants = List(
      Constant("msPerDay", DecimalMathValueLiteral(BigDecimal(86400000))),
    ),
    tables = Map(
      "table-x" -> Table(
        "table-x",
        List("Internal Slot"),
        List(List("[[Value]]")),
      ),
    ),
  )
  private lazy val compiler: Compiler = new Compiler(spec)

  /** metalanguage parser aware of the constants of the fixture specification */
  private lazy val langParser: Parsers =
    LangParser.withConstNames(spec.constantMap.keySet)

  /** parse a single metalanguage step
    *
    * The `upper` parser is needed because a step starts with an uppercase
    * letter, and `yetStep` is the fallback for unsupported steps.
    */
  private def parseStep(str: String): Step =
    import langParser.{given, *}
    langParser.parseBy(upper ~> (step | yetStep))(str)

  /** compile a single metalanguage step in a minimal algorithm and return the
    * compiled instruction with the newly created auxiliary functions
    */
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
      spec = spec,
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

  /** drop the information that the textual form of IR cannot express, because
    * the expected IR is parsed from it: the metalanguage types, parameters, and
    * algorithms kept for the backward edges to the specification, and the
    * abrupt-completion mark of `IIf`
    */
  private val normalizer = new IRWalker {
    override def walk(ty: IRType): IRType = IRType(ty.ty, None)
    override def walk(param: IRParam): IRParam =
      IRParam(walk(param.lhs), walk(param.ty), param.optional, None)
    override def walk(func: Func): Func =
      super.walk(func).copy(algo = None)
    override def walk(inst: Inst): Inst = super.walk(inst) match
      case IIf(cond, thenInst, elseInst, _) => IIf(cond, thenInst, elseInst)
      case inst                             => inst
  }

  /** metalanguage syntax covered by the cases above */
  private val covered: MSet[String] = MSet()
  private val coverageCollector = new LangUnitWalker {
    override def walk(step: Step): Unit =
      covered += step.getClass.getSimpleName; super.walk(step)
    override def walk(expr: Expression): Unit =
      covered += expr.getClass.getSimpleName; super.walk(expr)
    override def walk(cond: Condition): Unit =
      covered += cond.getClass.getSimpleName; super.walk(cond)
    override def walk(ref: Reference): Unit =
      covered += ref.getClass.getSimpleName; super.walk(ref)
  }

  /** check the IR instructions compiled from metalanguage steps */
  private def checkCompile(
    desc: String,
    needRetComp: Boolean = false,
    sdo: Boolean = false,
  )(cases: (String, String)*): Unit = check(desc) {
    var failed = 0
    for ((snippet, expected) <- cases) {
      val step = parseStep(snippet)
      coverageCollector.walk(step)
      val (inst, _) = compileStep(step, needRetComp, sdo)
      val result = normalizer.walk(inst)
      val expectedInst =
        normalizer.walk(Inst.from(s"{$LINE_SEP$expected$LINE_SEP}"))
      if (result != expectedInst) {
        failed += 1
        println(s"[FAILED] $desc")
        println(s"- step: $snippet")
        println(s"- expected: $expected")
        println(s"- result: $inst")
      }
    }
    // NOTE: all the cases are checked before failing to keep the coverage
    if (failed > 0) fail(s"$failed cases are not compiled as expected")
  }

  /** check the auxiliary IR functions compiled from metalanguage steps */
  private def checkCompileFuncs(desc: String)(
    cases: (String, String)*,
  ): Unit = check(desc) {
    var failed = 0
    for ((snippet, expected) <- cases) {
      val step = parseStep(snippet)
      coverageCollector.walk(step)
      val (_, funcs) = compileStep(step, false, false)
      val result = funcs.map(normalizer.walk)
      val expectedFuncs = List(normalizer.walk(Func.from(expected)))
      if (result != expectedFuncs) {
        failed += 1
        println(s"[FAILED] $desc")
        println(s"- step: $snippet")
        println(s"- expected: $expected")
        println(s"- result: ${funcs.mkString(LINE_SEP)}")
      }
    }
    if (failed > 0) fail(s"$failed cases are not compiled as expected")
  }

  /** check whether all the metalanguage syntax is covered by the cases above */
  private def checkCoverage: Unit = for {
    (category, names) <- List(
      "Step" -> leaves[Step],
      "Expression" -> leaves[Expression],
      "Condition" -> leaves[Condition],
      "Reference" -> leaves[Reference],
    )
  } check(s"$category coverage") {
    val missing = names.filterNot(covered.contains)
    if (missing.nonEmpty) fail(s"uncovered: ${missing.mkString(", ")}")
  }

  /** names of the leaf case classes of a sealed trait */
  private inline def leaves[T]: List[String] =
    summonFrom {
      case m: Mirror.SumOf[T]     => leavesOf[m.MirroredElemTypes]
      case m: Mirror.ProductOf[T] => List(constValue[m.MirroredLabel])
    }
  private inline def leavesOf[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => leaves[h] ++ leavesOf[t]

  init
}
