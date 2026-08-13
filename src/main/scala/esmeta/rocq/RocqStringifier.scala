package esmeta.rocq

import esmeta.LINE_SEP
import esmeta.ir.*

/** Signals that the current translation component is unsupported.
  *
  * This exception is caught only at a function-body boundary. Other exceptions
  * escape so generator bugs are not silently converted to fallback output.
  */
private[rocq] case class UnsupportedRocqTranslation(feature: String)
  extends RuntimeException(feature)

/** Semantic result of translating one IR function. */
enum RocqTranslationStatus {
  case Succeeded
  case Fallout(reasons: List[String])
}

/** Generated Rocq source together with its semantic translation status. */
case class RocqTranslation(source: String, status: RocqTranslationStatus)

/** A minimal IR-to-Rocq interaction-tree generator.
  *
  * Every function argument, local value, expression result, and return value
  * uses the universal `IRValue` carrier. IR locals become ordinary lexical Rocq
  * binders. Stateful primitive operations are lifted into an ITree state monad,
  * while function calls become CRIS `callE` events.
  */
class RocqStringifier(funcs: Iterable[Func] = Nil) {
  private val functionsByName: Map[String, Func] =
    funcs.iterator.map(func => func.name -> func).toMap

  def apply(func: Func): String = translate(func).source

  /** Translate one IR function, falling back only for unsupported syntax. */
  def translate(func: Func): RocqTranslation =
    try
      RocqTranslation(
        stringify(func, translateBody(func)),
        RocqTranslationStatus.Succeeded,
      )
    catch {
      case unsupported: UnsupportedRocqTranslation =>
        val reasons = List(unsupported.feature)
        RocqTranslation(
          stringify(
            func = func,
            body = "itree_state_fail",
            falloutReasons = reasons,
          ),
          RocqTranslationStatus.Fallout(reasons),
        )
    }

  /** Translate a function body: a block that has to reach an `IReturn`. */
  private def translateBody(func: Func): String =
    s"itree_block_body (${translateBlock(List(func.body), 0)})"

  /** Translate an instruction sequence into an `ITree_Block`.
    *
    * `branch` counts the branches already opened along this path. It exists
    * only to keep generated binder names distinct: shadowing would be harmless,
    * since every binder is consumed by the application that immediately
    * encloses it, but distinct names keep the generated Rocq readable once a
    * proof unfolds it. Sibling blocks may reuse a number because their scopes
    * are disjoint.
    */
  private def translateBlock(insts: List[Inst], branch: Int): String =
    insts match {
      case Nil =>
        "itree_block_fallthrough"
      case ISeq(nested) :: rest =>
        translateBlock(nested ::: rest, branch)
      case IReturn(expr) :: _ =>
        s"itree_block_return (${translateExpr(expr)})"
      case ILet(lhs, expr) :: rest =>
        stateBind(
          RocqNaming.local(lhs),
          translateExpr(expr),
          translateBlock(rest, branch),
        )
      case IExpr(expr) :: rest =>
        stateThen(translateExpr(expr), translateBlock(rest, branch))
      case INop() :: rest =>
        translateBlock(rest, branch)
      // `isAbruptInst` only records why the extractor emitted this branch, so it
      // carries no semantics to translate.
      case IIf(cond, thenInst, elseInst, _) :: rest =>
        translateIf(cond, thenInst, elseInst, rest, branch)
      case IAssign(_, _) :: _ =>
        unsupported("instruction IAssign")
      // The base reference reuses the `ERef` translation, so an unsupported
      // reference kind falls out with the same reason it would in an expression.
      case IExpand(base, field) :: rest =>
        stateBind(
          "expand_base'",
          translateExpr(ERef(base)),
          stateBind(
            "expand_field'",
            translateExpr(field),
            stateThen(
              liftState("expand_record_field expand_base' expand_field'"),
              translateBlock(rest, branch),
            ),
          ),
        )
      case IDelete(_, _) :: _ =>
        unsupported("instruction IDelete")
      // The interpreter evaluates the pushed element before the list.
      case IPush(elem, list, front) :: rest =>
        stateBind(
          "push_value'",
          translateExpr(elem),
          stateBind(
            "push_list'",
            translateExpr(list),
            stateThen(
              liftState(s"list_push push_list' push_value' ${rocqBool(front)}"),
              translateBlock(rest, branch),
            ),
          ),
        )
      case IPop(lhs, list, front) :: rest =>
        stateBind(
          "pop_list'",
          translateExpr(list),
          stateBind(
            RocqNaming.local(lhs),
            liftState(s"list_pop pop_list' ${rocqBool(front)}"),
            translateBlock(rest, branch),
          ),
        )
      case IAssert(_) :: _ =>
        unsupported("instruction IAssert")
      case IPrint(_) :: _ =>
        unsupported("instruction IPrint")
      case IWhile(_, _) :: _ =>
        unsupported("instruction IWhile")
      case ICall(lhs, EClo(fname, Nil), args) :: rest =>
        translateCall(lhs, fname, args, rest, branch)
      case ICall(_, EClo(_, _), _) :: _ =>
        unsupported("instruction ICall with captured closure")
      case ICall(_, _, _) :: _ =>
        unsupported("instruction ICall with dynamic callee")
      case ISdoCall(_, _, _, _) :: _ =>
        unsupported("instruction ISdoCall")
    }

  /** Translate a branch, then continue with whatever follows it.
    *
    * The continuation is emitted once after the branch instead of being copied
    * into both arms: `itree_block_seq` runs it only when the taken arm falls
    * through. Copying would be exponential in the nesting depth, and ECMA-262
    * abstract operations nest branches freely.
    */
  private def translateIf(
    cond: Expr,
    thenInst: Inst,
    elseInst: Inst,
    rest: List[Inst],
    branch: Int,
  ): String = {
    val binder = s"branch_condition_$branch'"
    val branched =
      s"itree_block_if $binder" + LINE_SEP +
      indent(s"(${translateBlock(List(thenInst), branch + 1)})") + LINE_SEP +
      indent(s"(${translateBlock(List(elseInst), branch + 1)})")
    val continued =
      if (rest.isEmpty) branched
      else
        "itree_block_seq" + LINE_SEP +
        indent(s"($branched)") + LINE_SEP +
        indent(s"(${translateBlock(rest, branch + 1)})")
    stateBind(binder, translateExpr(cond), continued)
  }

  /** Evaluate call arguments from left to right, then emit a typed CRIS call
    * event. Function signatures contain names and types only, so this creates
    * no dependency on the callee implementation.
    */
  private def translateCall(
    lhs: Local,
    fname: String,
    args: List[Expr],
    rest: List[Inst],
    branch: Int,
  ): String = {
    def loop(
      remaining: List[Expr],
      index: Int,
      values: List[String],
    ): String = remaining match {
      case Nil =>
        val arguments = values.foldRight("nil") { (value, tail) =>
          s"cons $value ($tail)"
        }
        stateBind(
          RocqNaming.local(lhs),
          s"itree_state_call ${signature(fname)} ($arguments)",
          translateBlock(rest, branch),
        )
      case expr :: tail =>
        val binder = s"call_argument_$index'"
        stateBind(
          binder,
          translateExpr(expr),
          loop(tail, index + 1, values :+ binder),
        )
    }

    loop(args, 0, Nil)
  }

  /** Resolve a raw IR callee name to its kind-qualified signature. */
  private def signature(fname: String): String =
    functionsByName.get(fname) match {
      case Some(func) => RocqNaming.signature(func)
      case None       => unsupported(s"call to unknown function: $fname")
    }

  /** Translate the initially supported expression subset to an IRValue
    * computation.
    */
  private def translateExpr(expr: Expr): String = expr match {
    case ERef(local @ Name(_)) =>
      stateReturn(RocqNaming.local(local))
    case ERef(local @ Temp(_)) =>
      stateReturn(RocqNaming.local(local))
    case ERef(Global(_)) =>
      unsupported("expression ERef with a global or field reference")
    case ERef(Field(_, _)) =>
      unsupported("expression ERef with a global or field reference")
    case EUnary(uop, operand) =>
      translateUnary(uop, operand)
    case EBinary(bop, left, right) =>
      translateBinary(bop, left, right)
    case ERecord(name, fields) =>
      translateRecord(name, fields)
    case EBigInt(value) =>
      stateReturn(s"IR_ESValue (BigintV ((${value.toString})%Z))")
    case EStr(value) =>
      stateReturn(s"IR_ESValue (StrV (${translateString(value)}))")
    case EBool(value) =>
      stateReturn(if (value) "op_true" else "op_false")
    case EUndef() =>
      stateReturn("IR_undefined")
    case ENull() =>
      stateReturn("IR_null")
    case EEnum(name) =>
      stateReturn(s"IR_Enum (${translateString(name)})")
    case EParse(_, _) =>
      unsupported("expression EParse")
    case EGrammarSymbol(_, _) =>
      unsupported("expression EGrammarSymbol")
    case ESourceText(_) =>
      unsupported("expression ESourceText")
    case EYet(_) =>
      unsupported("expression EYet")
    case EContains(_, _) =>
      unsupported("expression EContains")
    case ESubstring(_, _, _) =>
      unsupported("expression ESubstring")
    case ETrim(_, _) =>
      unsupported("expression ETrim")
    case EVariadic(_, _) =>
      unsupported("expression EVariadic")
    case EMathOp(_, _) =>
      unsupported("expression EMathOp")
    case EConvert(_, _) =>
      unsupported("expression EConvert")
    case EExists(_) =>
      unsupported("expression EExists")
    case ETypeOf(_) =>
      unsupported("expression ETypeOf")
    case EInstanceOf(_, _) =>
      unsupported("expression EInstanceOf")
    case ETypeCheck(_, _) =>
      unsupported("expression ETypeCheck")
    case ESizeOf(_) =>
      unsupported("expression ESizeOf")
    case EClo(_, _) =>
      unsupported("expression EClo")
    case ECont(_) =>
      unsupported("expression ECont")
    case EDebug(_) =>
      unsupported("expression EDebug")
    case ERandom() =>
      unsupported("expression ERandom")
    case ESyntactic(_, _, _, _) =>
      unsupported("expression ESyntactic")
    case ELexical(_, _) =>
      unsupported("expression ELexical")
    case EMap(_, _) =>
      unsupported("expression EMap")
    case EList(exprs) =>
      translateList(exprs)
    case ECopy(_) =>
      unsupported("expression ECopy")
    case EKeys(_, _) =>
      unsupported("expression EKeys")
    case EMath(_) =>
      unsupported("expression EMath")
    case EInfinity(_) =>
      unsupported("expression EInfinity")
    case ENumber(_) =>
      unsupported("expression ENumber")
    case ECodeUnit(_) =>
      unsupported("expression ECodeUnit")
  }

  /** Translate a unary operation via the dynamic definitions in `op.v`. */
  private def translateUnary(uop: UOp, operandExpr: Expr): String = {
    val operation = uop match {
      case UOp.Neg   => "op_neg"
      case UOp.Not   => "op_not"
      case UOp.Abs   => unsupported("unary operator Abs")
      case UOp.Floor => unsupported("unary operator Floor")
      case UOp.BNot  => unsupported("unary operator BNot")
    }
    stateBind(
      "operand_value",
      translateExpr(operandExpr),
      liftState(s"$operation operand_value"),
    )
  }

  /** Translate a binary operation.
    *
    * `And` and `Or` remain translation rules because evaluating their right
    * operand is conditional. Eager operators delegate their run-time type
    * checks and behavior to `op.v`.
    */
  private def translateBinary(
    bop: BOp,
    leftExpr: Expr,
    rightExpr: Expr,
  ): String = bop match {
    case BOp.And =>
      val right = translateExpr(rightExpr)
      val continuation =
        "match left_value with" + LINE_SEP +
        s"| IR_ESValue (BoolV trueB) => ($right)" + LINE_SEP +
        s"| IR_ESValue (BoolV falseB) => ${stateReturn("op_false")}" + LINE_SEP +
        "| _ => itree_state_fail" + LINE_SEP +
        "end"
      stateBind("left_value", translateExpr(leftExpr), continuation)
    case BOp.Or =>
      val right = translateExpr(rightExpr)
      val continuation =
        "match left_value with" + LINE_SEP +
        s"| IR_ESValue (BoolV trueB) => ${stateReturn("op_true")}" + LINE_SEP +
        s"| IR_ESValue (BoolV falseB) => ($right)" + LINE_SEP +
        "| _ => itree_state_fail" + LINE_SEP +
        "end"
      stateBind("left_value", translateExpr(leftExpr), continuation)
    case BOp.Add =>
      translateEagerBinary("op_add", leftExpr, rightExpr)
    case BOp.Sub =>
      translateEagerBinary("op_sub", leftExpr, rightExpr)
    case BOp.Mul =>
      translateEagerBinary("op_mul", leftExpr, rightExpr)
    case BOp.Lt =>
      translateEagerBinary("op_lt", leftExpr, rightExpr)
    case BOp.Equal =>
      translateEagerBinary("op_equal", leftExpr, rightExpr)
    case BOp.Eq =>
      translateEagerBinary("op_eq", leftExpr, rightExpr)
    case BOp.Xor =>
      translateEagerBinary("op_xor", leftExpr, rightExpr)
    case BOp.Pow =>
      unsupported("binary operator Pow")
    case BOp.Div =>
      unsupported("binary operator Div")
    case BOp.Mod =>
      unsupported("binary operator Mod")
    case BOp.BAnd =>
      unsupported("binary operator BAnd")
    case BOp.BOr =>
      unsupported("binary operator BOr")
    case BOp.BXOr =>
      unsupported("binary operator BXOr")
    case BOp.LShift =>
      unsupported("binary operator LShift")
    case BOp.RShift =>
      unsupported("binary operator RShift")
  }

  /** Translate an eager binary operation after evaluating both operands. */
  private def translateEagerBinary(
    operation: String,
    leftExpr: Expr,
    rightExpr: Expr,
  ): String =
    stateBind(
      "left_value",
      translateExpr(leftExpr),
      stateBind(
        "right_value",
        translateExpr(rightExpr),
        liftState(s"$operation left_value right_value"),
      ),
    )

  /** Render a Rocq `bool` literal for an IR flag. */
  private def rocqBool(value: Boolean): String = if (value) "true" else "false"

  /** Allocate an ESMeta list in the generic IR heap.
    *
    * Elements are evaluated left to right before the cell is allocated, so the
    * allocation order matches the interpreter's.
    */
  private def translateList(exprs: List[Expr]): String = {
    def loop(
      remaining: List[Expr],
      index: Int,
      values: List[String],
    ): String = remaining match {
      case Nil =>
        val elements = values.foldRight("nil") { (value, tail) =>
          s"cons $value ($tail)"
        }
        liftState(s"allocate_list ($elements)")
      case expr :: tail =>
        val binder = s"list_element_$index'"
        stateBind(
          binder,
          translateExpr(expr),
          loop(tail, index + 1, values :+ binder),
        )
    }

    loop(exprs, 0, Nil)
  }

  /** Allocate an ESMeta record in the generic IR heap.
    *
    * Completion Records and Property Descriptors use this same path with their
    * respective type names; neither has a dedicated Rocq value constructor.
    */
  private def translateRecord(
    name: String,
    fields: List[(String, Expr)],
  ): String = {
    def loop(
      remaining: List[(String, Expr)],
      index: Int,
      values: List[(String, String)],
    ): String = remaining match {
      case Nil =>
        val fieldList = values.foldRight("nil") {
          case ((fieldName, value), tail) =>
            s"cons ((${translateString(fieldName)}), $value) ($tail)"
        }
        liftState(s"allocate_record (${translateString(name)}) ($fieldList)")
      case (fieldName, expr) :: rest =>
        // The trailing quote cannot be produced by RocqNaming.encode, keeping
        // this generated binder hygienic without a translation context.
        val binder = s"record_field_$index'"
        stateBind(
          binder,
          translateExpr(expr),
          loop(rest, index + 1, values :+ (fieldName -> binder)),
        )
    }

    loop(fields, 0, Nil)
  }

  /** Encode an 8-bit Rocq string without relying on literal escaping. */
  private def translateString(value: String): String =
    value.reverseIterator.foldLeft("EmptyString") { (tail, char) =>
      if (char.toInt > 0xff)
        unsupported(
          f"non-8-bit string code unit U+${char.toInt}%04X in EStr",
        )
      s"String (${translateAscii(char)}) ($tail)"
    }

  /** Encode one Rocq `ascii`, whose constructor stores low-to-high bits. */
  private def translateAscii(char: Char): String = {
    val bits = (0 until 8).map { bit =>
      if (((char.toInt >> bit) & 1) == 1) "true" else "false"
    }
    s"Ascii ${bits.mkString(" ")}"
  }

  private def stateReturn(value: String): String =
    s"itree_state_return ($value)"

  private def liftState(computation: String): String =
    s"itree_state_lift ($computation)"

  private def stateBind(
    binder: String,
    computation: String,
    continuation: String,
  ): String =
    s"itree_state_bind ($computation) (fun $binder =>" + LINE_SEP +
    indent(continuation) + LINE_SEP + ")"

  /** Sequence a computation whose result is intentionally discarded. */
  private def stateThen(computation: String, continuation: String): String =
    stateBind("_", computation, continuation)

  private def indent(source: String): String =
    source.linesIterator.map("  " + _).mkString(LINE_SEP)

  /** Abort translation of the current component as unsupported. */
  protected final def unsupported(feature: String): Nothing =
    throw UnsupportedRocqTranslation(feature)

  /** Emit one complete, universally typed function definition. */
  private def stringify(
    func: Func,
    body: String,
    falloutReasons: List[String] = Nil,
  ): String = {
    val parameters = func.params.zipWithIndex.map { (param, index) =>
      RocqNaming.parameter(param, index)
    }
    val argumentPattern = parameters.foldRight("nil") { (parameter, tail) =>
      s"cons $parameter ($tail)"
    }
    val call =
      (RocqNaming.function(func) :: parameters ::: List("itree_state'"))
        .mkString(" ")
    val builder = StringBuilder()
    builder
      .append("Require Import type manual_type op itree_state Signatures.")
      .append(LINE_SEP)
    builder
      .append("From Stdlib Require Import ZArith String Ascii.")
      .append(LINE_SEP)
    builder.append("From CRIS Require Import CRIS.").append(LINE_SEP)
    builder.append(LINE_SEP)
    builder.append("Definition ").append(RocqNaming.function(func))
    builder.append(" `{Σ : GRA}")
    for ((param, index) <- func.params.zipWithIndex)
      builder
        .append(LINE_SEP)
        .append("    (")
        .append(RocqNaming.parameter(param, index))
        .append(" : IRValue)")
    builder
      .append(LINE_SEP)
      .append("    : ITree_State_Completion IRValue :=")
      .append(LINE_SEP)
    if (falloutReasons.nonEmpty)
      builder
        .append("  (* Unsupported Rocq translation: ")
        .append(falloutReasons.mkString("; "))
        .append(".  Use the fallback computation. *)")
        .append(LINE_SEP)
    builder.append("  ").append(body).append(".").append(LINE_SEP)
    builder.append(LINE_SEP)
    builder
      .append("Definition ")
      .append(RocqNaming.semantic(func))
      .append(" `{Σ : GRA}")
      .append(LINE_SEP)
      .append("    : Any.t -> itree crisE Any.t :=")
      .append(LINE_SEP)
      .append("  cfunU ")
      .append(RocqNaming.signature(func))
      .append(" (fun itree_input' =>")
      .append(LINE_SEP)
      .append("    let '(itree_arguments', itree_state') := itree_input' in")
      .append(LINE_SEP)
      .append("    match itree_arguments' with")
      .append(LINE_SEP)
      .append("    | ")
      .append(argumentPattern)
      .append(" => ")
      .append(call)
      .append(LINE_SEP)
      .append("    | _ => Ret FAIL")
      .append(LINE_SEP)
      .append("    end).")
      .append(LINE_SEP)
    builder.toString
  }

  /** Generate the names and homogeneous types used by CRIS call events. */
  def signatures(funcs: Iterable[Func]): String = {
    val builder = StringBuilder()
    builder.append("Require Import itree_state.").append(LINE_SEP)
    builder.append("From Stdlib Require Import String.").append(LINE_SEP)
    builder.append("From CRIS Require Import CRIS.").append(LINE_SEP)
    builder.append(LINE_SEP)
    for (func <- funcs)
      builder
        .append("Definition ")
        .append(RocqNaming.signature(func))
        .append(" : fnsig_t IRFunctionInput IRFunctionOutput :=")
        .append(LINE_SEP)
        .append("  fnsig \"")
        .append(RocqNaming.signatureName(func))
        .append("\" ir_function_type.")
        .append(LINE_SEP)
    builder.toString
  }

  /** Generate the CRIS module that registers every function implementation. */
  def program(funcs: Iterable[Func]): String = {
    val list = funcs.toList
    val imports =
      "Require Export itree_state Signatures." :: list.map { func =>
        s"Require Export ${RocqNaming.module(func)}."
      }
    val fnsems =
      if (list.isEmpty) "  ∅"
      else
        list.zipWithIndex
          .map { (func, index) =>
            val prefix = if (index == 0) "  {[ " else "      "
            val suffix = if (index == list.size - 1) "]}" else ";"
            prefix +
            s"fid ${RocqNaming.signature(func)} #" + LINE_SEP +
            "      (msk_scp scopes msk_true," + LINE_SEP +
            s"       (fsp_none, ${RocqNaming.semantic(func)}))$suffix"
          }
          .mkString(LINE_SEP)

    (imports ::: List(
      "From Stdlib Require Import String Ascii.",
      "From CRIS Require Import CRIS.",
      "",
      "Module ESMetaProgram. Section ESMetaProgram.",
      "  Context `{!crisG Γ Σ α β τ _S _I}.",
      "",
      s"  Definition scopes : list string := cons (${translateString("ESMeta")}) nil.",
      "",
      "  Definition fnsems : fnsemmap :=",
      fnsems + ".",
      "",
      "  Program Definition smod : SMod.t := {|",
      "    SMod.scopes := scopes;",
      "    SMod.fnsems := fnsems;",
      "    SMod.initial_st := ∅;",
      "  |}.",
      "  Solve All Obligations with mod_tac.",
      "",
      "  Definition t : Mod.t := SMod.to_mod ∅ smod.",
      "End ESMetaProgram. End ESMetaProgram.",
      "",
    )).mkString(LINE_SEP)
  }
}

object RocqStringifier {
  def apply(): RocqStringifier = new RocqStringifier
}
