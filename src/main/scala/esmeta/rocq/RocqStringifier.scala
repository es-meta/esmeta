package esmeta.rocq

import esmeta.LINE_SEP
import esmeta.ir.*

/** Signals that the current translation component is unsupported. */
private[rocq] case class UnsupportedRocqTranslation(feature: String)
  extends RuntimeException(feature)

/** A shallow IR-to-Rocq interaction-tree generator. */
class RocqStringifier(
  funcs: Iterable[Func] = Nil,
  proofObligations: Boolean = false,
) {
  private val allFunctions = funcs.toList

  def apply(func: Func): String = header + translate(func)

  def translate(func: Func): String = Compiler().function(func)

  def program(funcs: Iterable[Func] = allFunctions): String = {
    val list = funcs.toList
    val definitions = list.map(func => Compiler().function(func))
    val lookup = programLookup(list)
    val main = programMain(list)
    (header :: definitions ::: List(lookup, main)).mkString(LINE_SEP)
  }

  private val header: String = {
    val stdlib =
      if (proofObligations)
        "From Stdlib Require Import List String ZArith NArith Program Lia."
      else "From Stdlib Require Import List String ZArith NArith."
    val setup =
      if (proofObligations) List("Local Obligation Tactic := idtac.")
      else Nil
    (List(
      stdlib,
      "From ITree Require Import Core.ITreeDefinition.",
      "From ESMeta Require Import ITreeIR.",
      "Import ListNotations.",
      "Open Scope string_scope.",
      "Open Scope ir_scope.",
    ) ++ setup ++ List("")).mkString(LINE_SEP)
  }

  private def programLookup(funcs: List[Func]): String = {
    val branches = funcs.map { func =>
      s"if String.eqb name (${stringLit(func.name)}) then " +
      s"Some ${RocqNaming.function(func)} else"
    }
    val body = (branches :+ "None").mkString(LINE_SEP + "    ")
    s"""Definition ir_program_lookup (name : string) : option IRFunction :=
       |  $body.
       |""".stripMargin
  }

  private def programMain(funcs: List[Func]): String =
    funcs.filter(_.main) match {
      case Nil =>
        """Definition ir_program_main : option IRFunction := None.
          |
          |Definition ir_main_itree : option (itree esmetaE (IRResult IRValue)) :=
          |  None.
          |""".stripMargin
      case main :: Nil =>
        val function = RocqNaming.function(main)
        s"""Definition ir_program_main : option IRFunction :=
           |  Some $function.
           |
           |Definition ir_main_itree : option (itree esmetaE (IRResult IRValue)) :=
           |  Some ($function []).
           |""".stripMargin
      case _ =>
        throw UnsupportedRocqTranslation(
          "an IR program must contain at most one main function",
        )
    }

  private class Compiler {
    private var assertionObligations = 0

    def function(func: Func): String = {
      val parameters = listTerm(func.params.map(param => local(param.lhs)))
      val body = block(List(func.body), "True")
      val keyword =
        if (proofObligations) "Program Definition" else "Definition"
      val definition =
        s"""$keyword ${RocqNaming.function(func)} : IRFunction :=
           #  ir_make_function $parameters
           #    (
           #${indent(body, 6)}
           #    ).
           #""".stripMargin('#')
      val proofs = List
        .fill(assertionObligations)(
          """Next Obligation.
            |  intros.
            |  lia.
            |Qed.
            |""".stripMargin,
        )
        .mkString(LINE_SEP)
      definition + proofs
    }

    private def block(insts: List[Inst], pathCondition: String): String =
      insts match {
        case Nil =>
          "ir_skip"
        case ISeq(nested) :: rest =>
          block(nested ::: rest, pathCondition)
        case IReturn(expr) :: _ =>
          s"ir_return ${computationArgument(expression(expr))}"
        case ILet(lhs, expr) :: rest =>
          thenUnit(
            writeLocal(local(lhs), expression(expr)),
            block(rest, pathCondition),
          )
        case IAssign(localValue: Local, expr) :: rest =>
          thenUnit(
            writeLocal(local(localValue), expression(expr)),
            block(rest, pathCondition),
          )
        case IAssign(ref, expr) :: rest =>
          thenUnit(
            writeTarget(target(ref), expression(expr)),
            block(rest, pathCondition),
          )
        case IExpand(base, field) :: rest =>
          thenUnit(
            commandComp(
              "ref.expand",
              List(readReference(base), expression(field)),
            ),
            block(rest, pathCondition),
          )
        case IDelete(base, field) :: rest =>
          thenUnit(
            commandComp(
              "ref.delete",
              List(readReference(base), expression(field)),
            ),
            block(rest, pathCondition),
          )
        case IPush(elem, list, front) :: rest =>
          val push =
            s"ir_push ${computationArgument(expression(elem))} " +
            s"${computationArgument(expression(list))} ${rocqBool(front)}"
          thenUnit(push, block(rest, pathCondition))
        case IPop(lhs, list, front) :: rest =>
          val pop =
            s"ir_pop ${computationArgument(expression(list))} ${rocqBool(front)}"
          thenUnit(writeLocal(local(lhs), pop), block(rest, pathCondition))
        case IAssert(EYet(_)) :: rest =>
          block(rest, pathCondition)
        case IAssert(expr) :: rest =>
          val obligation =
            if (proofObligations) {
              assertionObligations += 1
              val assertion = proposition(expr)
              s"($pathCondition -> $assertion) _"
            } else "True I"
          val assertion =
            s"ir_assert ${computationArgument(expression(expr))} " +
            obligation
          thenUnit(assertion, block(rest, pathCondition))
        case IPrint(expr) :: rest =>
          val print = s"ir_print ${computationArgument(expression(expr))}"
          thenUnit(print, block(rest, pathCondition))
        case INop() :: rest =>
          block(rest, pathCondition)
        case IIf(cond, thenInst, elseInst, _) :: rest =>
          val (thenPath, elsePath) =
            if (proofObligations) {
              val condition = proposition(cond)
              (
                s"($pathCondition /\\ $condition)",
                s"($pathCondition /\\ ~ $condition)",
              )
            } else (pathCondition, pathCondition)
          val conditional = conditionalCommand(
            expression(cond),
            block(List(thenInst), thenPath),
            block(List(elseInst), elsePath),
          )
          thenUnit(conditional, block(rest, pathCondition))
        case IWhile(cond, body) :: rest =>
          if (proofObligations)
            throw UnsupportedRocqTranslation(
              "assertion proof obligations for while require a loop invariant",
            )
          val loop = whileCommand(
            expression(cond),
            block(List(body), pathCondition),
          )
          thenUnit(loop, block(rest, pathCondition))
        case ICall(lhs, callee, args) :: rest =>
          val call =
            s"ir_call ${computationArgument(expression(callee))} " +
            computationList(args.map(expression))
          thenUnit(writeLocal(local(lhs), call), block(rest, pathCondition))
        case ISdoCall(lhs, base, operation, args) :: rest =>
          val call =
            s"ir_sdo_call ${computationArgument(expression(base))} " +
            s"(${stringLit(operation)}) ${computationList(args.map(expression))}"
          thenUnit(writeLocal(local(lhs), call), block(rest, pathCondition))
        case IExpr(expr) :: rest =>
          thenDo(expression(expr), block(rest, pathCondition))
      }

    private def proposition(expr: Expr): String = expr match {
      case EBool(true)              => "True"
      case EBool(false)             => "False"
      case EUnary(UOp.Not, operand) => s"(~ ${proposition(operand)})"
      case EBinary(BOp.And, left, right) =>
        s"(${proposition(left)} /\\ ${proposition(right)})"
      case EBinary(BOp.Or, left, right) =>
        s"(${proposition(left)} \\/ ${proposition(right)})"
      case EBinary(BOp.Lt, EMath(left), EMath(right)) =>
        s"(Z.lt ${integerMath(left)} ${integerMath(right)})"
      case EBinary(BOp.Equal, EMath(left), EMath(right)) =>
        s"(${integerMath(left)} = ${integerMath(right)})"
      case _ =>
        throw UnsupportedRocqTranslation(
          s"assertion proof expression is not supported yet: $expr",
        )
    }

    private def integerMath(value: BigDecimal): String =
      value.toBigIntExact match {
        case Some(integer) => zLit(integer)
        case None =>
          throw UnsupportedRocqTranslation(
            s"non-integer mathematical value in assertion proof: $value",
          )
      }

    private def expression(expr: Expr): String = expr match {
      case EParse(code, rule) =>
        primitiveComp("parse", List(expression(code), expression(rule)))
      case EGrammarSymbol(name, parameters) =>
        returnValue(
          s"IR_GrammarSymbol (${stringLit(name)}) ${boolList(parameters)}",
        )
      case ESourceText(base) =>
        primitiveComp("source-text", List(expression(base)))
      case EYet(message) =>
        primitiveComp("yet", List(returnValue(stringValue(message))))
      case EContains(list, elem) =>
        primitiveComp("contains", List(expression(list), expression(elem)))
      case ESubstring(base, from, to) =>
        primitiveComp(
          "substring",
          (base :: from :: to.toList).map(expression),
        )
      case ETrim(base, isStarting) =>
        primitiveComp(
          "trim",
          List(expression(base), returnValue(boolValue(isStarting))),
        )
      case ERef(ref) =>
        readReference(ref)
      case EUnary(operator, operand) =>
        unary(unaryName(operator), expression(operand))
      case EBinary(BOp.And, left, right) =>
        logical("and", expression(left), expression(right))
      case EBinary(BOp.Or, left, right) =>
        logical("or", expression(left), expression(right))
      case EBinary(operator, left, right) =>
        binary(
          binaryName(operator),
          expression(left),
          expression(right),
        )
      case EVariadic(operator, exprs) =>
        variadic(variadicName(operator), exprs.map(expression))
      case EMathOp(operator, exprs) =>
        mathOp(operator.toString.toLowerCase, exprs.map(expression))
      case EConvert(operator, base) =>
        val (name, extra) = operator match {
          case COp.ToApproxNumber => ("to-approx-number", Nil)
          case COp.ToNumber       => ("to-number", Nil)
          case COp.ToBigInt       => ("to-bigint", Nil)
          case COp.ToMath         => ("to-math", Nil)
          case COp.ToCodeUnit     => ("to-code-unit", Nil)
          case COp.ToStr(radix)   => ("to-string", radix.toList)
        }
        convert(name, (base :: extra).map(expression))
      case EExists(localValue: Local) =>
        s"ir_local_exists (${local(localValue)})"
      case EExists(ref) =>
        s"ir_target_exists ${computationArgument(target(ref))}"
      case ETypeOf(base) =>
        primitiveComp("type-of", List(expression(base)))
      case EInstanceOf(base, target) =>
        primitiveComp(
          "instance-of",
          List(expression(base), expression(target)),
        )
      case ETypeCheck(base, ty) =>
        primitiveComp(
          "type-check",
          List(expression(base), returnValue(stringValue(ty.toString))),
        )
      case ESizeOf(base) =>
        primitiveComp("size-of", List(expression(base)))
      case EClo(name, captured) =>
        s"ir_capture_closure (${stringLit(name)}) " +
        listTerm(captured.map(local))
      case ECont(name) =>
        s"ir_capture_continuation (${stringLit(name)})"
      case EDebug(base) =>
        primitiveComp("debug", List(expression(base)))
      case ERandom() =>
        primitiveComp("random", Nil)
      case ESyntactic(name, arguments, rhsIndex, children) =>
        s"ir_syntactic (${stringLit(name)}) ${boolList(arguments)} $rhsIndex " +
        optionalComputationList(children.toList)
      case ELexical(name, base) =>
        s"ir_lexical (${stringLit(name)}) " +
        computationArgument(expression(base))
      case ERecord(typeName, fields) =>
        val arguments = fields.flatMap {
          case (name, value) =>
            List(returnValue(stringValue(name)), expression(value))
        }
        primitiveComp(
          "alloc.record",
          returnValue(stringValue(typeName)) :: arguments,
        )
      case EMap(_, pairs) =>
        primitiveComp(
          "alloc.map",
          pairs.flatMap {
            case (key, value) =>
              List(expression(key), expression(value))
          },
        )
      case EList(exprs) =>
        primitiveComp("alloc.list", exprs.map(expression))
      case ECopy(base) =>
        primitiveComp("alloc.copy", List(expression(base)))
      case EKeys(base, intSorted) =>
        primitiveComp(
          "alloc.keys",
          List(expression(base), returnValue(boolValue(intSorted))),
        )
      case EMath(value) =>
        returnValue(mathValue(value))
      case EInfinity(positive) =>
        returnValue(s"IR_Infinity ${rocqBool(positive)}")
      case ENumber(value) =>
        returnValue(numberValue(value))
      case EBigInt(value) =>
        returnValue(s"IR_BigInt ${zLit(value)}")
      case EStr(value) =>
        returnValue(stringValue(value))
      case EBool(value) =>
        returnValue(boolValue(value))
      case EUndef() =>
        returnValue("IR_Undefined")
      case ENull() =>
        returnValue("IR_Null")
      case EEnum(name) =>
        returnValue(s"IR_Enum (${stringLit(name)})")
      case ECodeUnit(value) =>
        returnValue(s"IR_CodeUnit (${value.toInt}%N)")
    }

    private def target(ref: Ref): String = ref match {
      case localValue: Local =>
        returnTarget(s"IR_LocalTarget (${local(localValue)})")
      case Global(name) =>
        returnTarget(s"IR_GlobalTarget (${stringLit(name)})")
      case Field(base, field) =>
        s"ir_field_target ${computationArgument(readReference(base))} " +
        computationArgument(expression(field))
    }

    private def readReference(ref: Ref): String = ref match {
      case localValue: Local =>
        s"ir_read_local (${local(localValue)})"
      case _ =>
        s"ir_read_target ${computationArgument(target(ref))}"
    }

    private def thenDo(computation: String, continuation: String): String =
      s"${sequenceOperand(computation)} ;;;" + LINE_SEP + continuation

    private def thenUnit(computation: String, continuation: String): String =
      if (continuation == "ir_skip") computation
      else thenDo(computation, continuation)

    private def conditionalCommand(
      condition: String,
      thenComputation: String,
      elseComputation: String,
    ): String =
      s"ir_if ${computationArgument(condition)} then" + LINE_SEP +
      indent(thenComputation) + LINE_SEP +
      "else" + LINE_SEP +
      indent(elseComputation) + LINE_SEP +
      "end"

    private def writeLocal(local: String, computation: String): String =
      s"ir_write_local ($local) ${computationArgument(computation)}"

    private def writeTarget(target: String, value: String): String =
      s"ir_write_target ${computationArgument(target)} " +
      computationArgument(value)

    private def logical(
      operation: String,
      left: String,
      right: String,
    ): String =
      s"ir_logical_$operation ${computationArgument(left)} " +
      computationArgument(right)

    private def unary(operation: String, operand: String): String =
      s"ir_unary (${stringLit(operation)}) ${computationArgument(operand)}"

    private def binary(
      operation: String,
      left: String,
      right: String,
    ): String =
      s"ir_binary (${stringLit(operation)})" + LINE_SEP +
      indent(computationArgument(left)) + LINE_SEP +
      indent(computationArgument(right))

    private def variadic(operation: String, operands: List[String]): String =
      s"ir_variadic (${stringLit(operation)}) ${computationList(operands)}"

    private def mathOp(operation: String, operands: List[String]): String =
      s"ir_math (${stringLit(operation)}) ${computationList(operands)}"

    private def convert(operation: String, operands: List[String]): String =
      s"ir_convert (${stringLit(operation)}) ${computationList(operands)}"

    private def whileCommand(
      condition: String,
      body: String,
    ): String =
      s"ir_while ${computationArgument(condition)} do" + LINE_SEP +
      indent(body) + LINE_SEP +
      "end"

    private def sequenceOperand(computation: String): String =
      if (
        computation.startsWith("ir_if ") ||
        computation.startsWith("ir_while ")
      ) computation
      else grouped(computation)

    private def grouped(computation: String): String =
      if (computation.contains(LINE_SEP)) parenthesized(computation)
      else computation

    private def parenthesized(term: String): String =
      if (term.contains(LINE_SEP))
        "(" + LINE_SEP + indent(term) + LINE_SEP + ")"
      else s"($term)"

    private def computationArgument(computation: String): String =
      parenthesized(computation)

    private def computationList(computations: List[String]): String = {
      val elements = computations.map(computationArgument)
      if (elements.forall(!_.contains(LINE_SEP))) listTerm(elements)
      else
        "[" + LINE_SEP +
        indent(elements.mkString(";" + LINE_SEP)) + LINE_SEP +
        "]"
    }

    private def optionalComputationList(
      computations: List[Option[Expr]],
    ): String = {
      val elements = computations.map {
        case None       => "None"
        case Some(expr) => s"Some ${computationArgument(expression(expr))}"
      }
      if (elements.forall(!_.contains(LINE_SEP))) listTerm(elements)
      else
        "[" + LINE_SEP +
        indent(elements.mkString(";" + LINE_SEP)) + LINE_SEP +
        "]"
    }

    private def primitiveComp(
      operation: String,
      arguments: List[String],
    ): String =
      s"ir_primitive (${stringLit(operation)}) " +
      computationList(arguments)

    private def commandComp(
      operation: String,
      arguments: List[String],
    ): String =
      s"ir_command (${stringLit(operation)}) " +
      computationList(arguments)

    private def returnValue(value: String): String = s"ir_pure ($value)"

    private def returnTarget(value: String): String = s"ir_pure ($value)"

    private def local(localValue: Local): String = localValue match {
      case Name(name)  => s"IR_Name (${stringLit(name)})"
      case Temp(index) => s"IR_Temp $index"
    }
  }

  private def unaryName(operator: UOp): String = operator match {
    case UOp.Abs   => "abs"
    case UOp.Floor => "floor"
    case UOp.Neg   => "neg"
    case UOp.Not   => "not"
    case UOp.BNot  => "bit-not"
  }

  private def binaryName(operator: BOp): String = operator match {
    case BOp.Eq     => "eq"
    case BOp.Add    => "add"
    case BOp.Sub    => "sub"
    case BOp.Mul    => "mul"
    case BOp.Pow    => "pow"
    case BOp.Div    => "div"
    case BOp.Mod    => "mod"
    case BOp.Lt     => "lt"
    case BOp.Equal  => "equal"
    case BOp.BAnd   => "bit-and"
    case BOp.BOr    => "bit-or"
    case BOp.BXOr   => "bit-xor"
    case BOp.LShift => "left-shift"
    case BOp.RShift => "right-shift"
    case BOp.And    => "and"
    case BOp.Or     => "or"
    case BOp.Xor    => "xor"
  }

  private def variadicName(operator: VOp): String = operator match {
    case VOp.Min    => "min"
    case VOp.Max    => "max"
    case VOp.Concat => "concat"
  }

  private def mathValue(value: BigDecimal): String = {
    val decimal = value.bigDecimal.stripTrailingZeros
    val coefficient = BigInt(decimal.unscaledValue)
    val exponent = BigInt(-decimal.scale)
    s"IR_Math ${zLit(coefficient)} ${zLit(exponent)}"
  }

  private def numberValue(value: Double): String = {
    val signed = java.lang.Double.doubleToRawLongBits(value)
    val bits = BigInt(signed) & ((BigInt(1) << 64) - 1)
    s"IR_Number ($bits%N)"
  }

  private def zLit(value: BigInt): String =
    if (value < 0) s"(- (${-value}))%Z" else s"($value%Z)"

  private def rocqBool(value: Boolean): String =
    if (value) "true" else "false"

  private def boolValue(value: Boolean): String =
    s"IR_Bool ${rocqBool(value)}"

  private def stringValue(value: String): String =
    s"IR_String (${stringLit(value)})"

  private def boolList(values: Iterable[Boolean]): String =
    listTerm(values.map(rocqBool))

  private def listTerm(values: Iterable[String]): String =
    values.mkString("[", "; ", "]")

  private def stringLit(value: String): String = {
    value.find(_.toInt > 255).foreach { char =>
      throw UnsupportedRocqTranslation(
        f"non-8-bit string code unit U+${char.toInt}%04X",
      )
    }
    "\"" + value.replace("\"", "\"\"") + "\""
  }

  private def indent(source: String, spaces: Int = 2): String =
    source.linesIterator.map(" " * spaces + _).mkString(LINE_SEP)
}

object RocqStringifier {
  def apply(): RocqStringifier = new RocqStringifier
}
