package esmeta.es

import esmeta.LINE_SEP
import esmeta.lang.Step
import esmeta.lang.Type
import esmeta.spec.*
import esmeta.ty.*

/** polyfill code */
case class Polyfill(
  name: String,
  params: List[Param],
  body: Polyfill.Stmt,
  hasThis: Boolean = false,
  isAbstractOp: Boolean = false,
  aoImports: List[String] = Nil,
  tsNoCheck: Boolean = false,
) {
  override def toString: String =
    s"${banner}${importsToString}export function $preferedIdentifier ${headToString} ${body.toString}"

  def importsToString: String =
    if (aoImports.isEmpty) ""
    else
      aoImports
        .map(n => s"""import { AO__$n } from "./AO__$n.js";""")
        .mkString("", LINE_SEP, LINE_SEP + LINE_SEP)

  val banner: String =
    val TS = if (tsNoCheck) "// @ts-nocheck" else ""
    s"""|$TS
       |// THIS FILE IS AUTO-GENERATED, DO NOT EDIT
       |import type { Wrapped, BootStrap } from "@/model/type.js";
       |
       |""".stripMargin

  def headToString: String = {
    val receiver = if (hasThis) List(s"${Polyfill.THIS_PARAM} : Wrapped<unknown>") else Nil
    val paramStr =
      params.map { p =>
        val ts = Polyfill.tsParamType(p.ty)
        p.kind match
          case ParamKind.Normal   => s"${p.name} : $ts"
          case ParamKind.Optional => s"${p.name}? : $ts"
          case ParamKind.Variadic => s"...${p.name} : $ts[]"
      }
    (s"${Polyfill.RUNTIME} : BootStrap" :: receiver ::: paramStr).mkString("(", ", ", ")")
  }

  def preferedIdentifier: String =
    preferedFilename.stripSuffix(".ts").replace(".", "_")

  def preferedFilename: String =
    if (isAbstractOp) s"AO__${name}.ts"
    else if (name.startsWith("INTRINSICS.yet:"))
      s"${name.stripPrefix("INTRINSICS.yet:").replace("`", "").replace(".", "")}.ts"
    else s"${name}.ts"
}

object Polyfill {
  /** injected runtime parameter, threaded into every polyfill (and prefix for runtime ops) */
  val RUNTIME = "$"
  /** injected receiver parameter for BuiltinHead methods (the spec "this value") */
  val THIS_PARAM = "$this"

  // Map a spec parameter type to its TS type. Every value is Wrapped (so ops can
  // track it for taint/concolic). A List recurses into its element type; the
  // payload narrows when known (String/Number/integer/Boolean) and keeps a
  // `| undefined` union so `$.is(x, base(undefined))` guards narrow downstream
  // (e.g. GetSubstitution's `captures: Wrapped<string | undefined>[]`). Shared
  // with PolyfillGenerator, which casts AO-call args to their callee param type.
  def tsParamType(tpe: Type): String = tpe.ty match
    case vt: ValueTy if vt <= ListT => s"Wrapped<${tsPayload(vt.list.elem)}>[]"
    case vt: ValueTy                => s"Wrapped<${tsPayload(vt)}>"
    case _                          => "Wrapped<unknown>"

  // TS payload type inside Wrapped<...>; preserves a `| undefined` union.
  private def tsPayload(vt: ValueTy): String =
    val core = vt -- UndefT
    val base =
      if core.isBottom then ""
      else if core <= StrT then "string"
      else if core <= MathT || core <= NumberT then "number"
      else if core <= BoolT then "boolean"
      else "unknown"
    if base == "unknown" then "unknown"
    else if base.isEmpty then "undefined"
    else if vt.undef then s"$base | undefined"
    else base

  sealed trait Stmt {
    override def toString: String = toString(0)

    private val TAB = "  "
    def toString(depth: Int): String = (TAB * depth) + {
      this match
        case NormalStmt(code) => code
        case IfStmt(cond, thenStmt, elseStmt) =>
          s"if ($cond)" + LINE_SEP +
          s"${thenStmt.toString(depth)}" +
          (elseStmt match {
            case None => ""
            case Some(elseStmt) =>
              (TAB * depth) + "else" + LINE_SEP + elseStmt.toString(depth)
          })
        case WhileStmt(cond, body) =>
          s"while ($cond)" +
          LINE_SEP +
          s"${body.toString(depth)}"
        case ForEachStmt(index, end, body) =>
          s"for (var $index = 0; $index < $end; $index++)" +
          LINE_SEP +
          s"${body.toString(depth)}"
        case ForEachIntStmt(index, low, lowInc, high, highInc, true, body) =>
          val init = s"var $index = $low" + (if (lowInc) "" else " + 1")
          val cond = s"$index " + (if (highInc) "<=" else "=") + high
          s"for ($init; $cond; $index++)" + LINE_SEP + s"${body.toString(depth)}"
        case ForEachIntStmt(index, low, lowInc, high, highInc, false, body) =>
          val init = s"var $index = $low" + (if (lowInc) "" else " - 1")
          val cond = s"$index " + (if (highInc) ">=" else "=") + high
          s"for ($init; $cond; $index--)" + LINE_SEP + s"${body.toString(depth)}"
        case BlockStmt(stmts) =>
          "{" + LINE_SEP + stmts
            .map(_.toString(depth + 1))
            .mkString + (TAB * depth) + "}"
        case WrappedLetStmt(name, code, tryBody, catchBody) =>
          val tryBodyStr = tryBody.toString(depth + 1)
          val catchBodyStr = catchBody.toString(depth + 1)
          if (catchBodyStr.isBlank) {
            (TAB * (depth + 1)) + s"var $name = ${code.toString}" +
            tryBodyStr
          } else {
            "try" +
            LINE_SEP +
            (TAB * depth) + "{" +
            LINE_SEP +
            (TAB * (depth + 1)) + s"var $name = ${code.toString}" +
            tryBodyStr +
            (TAB * depth) + "}" +
            LINE_SEP +
            (TAB * depth) + s"catch(_${name}_abrupt)" +
            LINE_SEP + (TAB * depth) + "{" +
            LINE_SEP + catchBodyStr +
            LINE_SEP + (TAB * depth) + "}"
          }
        case NoOpStmt() => ""
        case CompoundStatement(stmts) =>
          stmts
            .filter(!_.isInstanceOf[NoOpStmt])
            .flatMap {
              case BlockStmt(innerStmts) => innerStmts.map(_.toString(depth))
              case x                     => x.toString(depth)
            }
            .mkString
            .trim
        case TryCatchStmt(tryStmt, catchVar, catchStmt) =>
          "try" +
          LINE_SEP +
          tryStmt.toString(depth) +
          (TAB * depth) + s"catch($catchVar)" +
          LINE_SEP + catchStmt.toString(depth)
    } + LINE_SEP

    def toList: List[Stmt] = this match {
      case BlockStmt(stmts)         => stmts
      case CompoundStatement(stmts) => stmts
      case stmt                     => List(stmt)
    }

    def ++(other: Stmt): Stmt = {
      new BlockStmt(this.toList ++ other.toList)
    }
  }

  // code
  case class NormalStmt(code: String) extends Stmt

  // if (cond) { thenStmt } else { elseStmt }
  case class IfStmt(cond: String, thenStmt: Stmt, elseStmt: Option[Stmt])
    extends Stmt

  // while (cond) { body }
  case class WhileStmt(cond: String, body: Stmt) extends Stmt

  // for (var index = 0; index < end; index++) { element = expr[index]; body }
  case class ForEachStmt(index: String, end: String, body: Stmt) extends Stmt

  // for (var index = start; index < end; index++) { body }
  case class ForEachIntStmt(
    index: String,
    low: String,
    lowInc: Boolean,
    high: String,
    highInc: Boolean,
    ascending: Boolean,
    body: Stmt,
  ) extends Stmt

  // { stmts }
  case class BlockStmt(stmts: List[Stmt]) extends Stmt

  // { stmts }
  case class TryCatchStmt(tryStmt: Stmt, catchVar: String, catchStmt: Stmt)
    extends Stmt

  // let x = expr
  case class WrappedLetStmt(
    name: String,
    code: Stmt,
    tryBody: Stmt,
    catchBody: Stmt,
  ) extends Stmt

  // NoOp
  case class NoOpStmt() extends Stmt

  // Compound Statement (No scope, only used in internal)
  case class CompoundStatement(stmts: List[Stmt]) extends Stmt

  // Additional Steps Declaration for Polyfill Extract
  sealed trait PolyfillStep()

  // LangStep
  case class LangStep(step: Step) extends PolyfillStep
}
