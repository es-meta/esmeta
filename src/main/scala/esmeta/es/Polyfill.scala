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
  numericImports: List[String] = Nil,
  isConstructor: Boolean = false,
) {
  override def toString: String =
    s"${banner}${importsToString}export function $preferedIdentifier ${headToString} ${body.toString}${markerToString}"

  def importsToString: String =
    val lines =
      // A constructor polyfill is stamped via markConstructable (see
      // markerToString) so the runtime permits `new`; import the helper.
      (if (isConstructor)
         List(
           s"""import { markConstructable } from "../internal/constructable.js";""",
         )
       else Nil) ++
      aoImports.map(n => s"""import { AO__$n } from "./AO__$n.js";""") ++
      // Numeric-method imports are already fully-qualified identifiers (`Number__equal`).
      numericImports.map(n => s"""import { $n } from "./$n.js";""")
    if (lines.isEmpty) ""
    else lines.mkString("", LINE_SEP, LINE_SEP + LINE_SEP)

  /** Mark a constructor polyfill so the runtime allows `new <fn>()`. Absent for
    * ordinary built-in functions, which the runtime then treats as
    * non-constructors (throwing on `new`, like native).
    */
  def markerToString: String =
    if (isConstructor)
      s"${LINE_SEP}markConstructable($preferedIdentifier);$LINE_SEP"
    else ""

  val banner: String =
    s"""|// THIS FILE IS AUTO-GENERATED, DO NOT EDIT
        |import type { SpecRuntime, Lifted, Unlifted } from "../type.js";
        |
        |""".stripMargin

  def headToString: String = {
    val receiver =
      if (hasThis) List(s"${Polyfill.THIS_PARAM} : Lifted<unknown>") else Nil
    val paramStr =
      params.map { p =>
        val ts = Polyfill.tsParamType(p.ty)
        p.kind match
          case ParamKind.Normal => s"${p.name} : $ts"
          case ParamKind.Optional =>
            s"${p.name} : $ts = ${Polyfill.RUNTIME}.default<undefined>(undefined, [])"
          case ParamKind.Variadic => s"...${p.name} : $ts[]"
      }
    (s"${Polyfill.RUNTIME} : SpecRuntime" :: receiver ::: paramStr)
      .mkString("(", ", ", ")")
  }

  def preferedIdentifier: String =
    preferedFilename.stripSuffix(".ts").replace(".", "_")

  def preferedFilename: String =
    val normalizedName = name
      .replace("%", "_")
      .replace("[", "_")
      .replace("]", "_")
      // numeric methods are named `Number::equal`; `::` is not a legal JS
      // identifier, so render them as `Number__equal` (matching the call site).
      .replace("::", "__")
    if (isAbstractOp) s"AO__${normalizedName}.ts"
    else if (normalizedName.startsWith("INTRINSICS.yet:"))
      s"${normalizedName.stripPrefix("INTRINSICS.yet:").replace("`", "").replace(".", "")}.ts"
    else s"${normalizedName}.ts"
}

object Polyfill {

  /** injected runtime parameter, threaded into every polyfill (and prefix for
    * runtime ops)
    */
  val RUNTIME = "$"

  /** injected receiver parameter for BuiltinHead methods (the spec "this
    * value")
    */
  val THIS_PARAM = "$this"

  // Map a spec parameter type to its TS type. Every value is Lifted (so ops can
  // track it for taint/concolic). A List recurses into its element type; the
  // payload narrows when known (String/Number/integer/Boolean) and keeps a
  // `| undefined` union so `$.is(x, base(undefined))` guards narrow downstream
  // (e.g. GetSubstitution's `captures: Lifted<string | undefined>[]`). Shared
  // with PolyfillGenerator, which casts AO-call args to their callee param type.
  def tsParamType(tpe: Type): String = tpe.ty match
    case vt: ValueTy if vt <= ListT => s"Lifted<${tsPayload(vt.list.elem)}>[]"
    case vt: ValueTy                => s"Lifted<${tsPayload(vt)}>"
    case _                          => "Lifted<unknown>"

  // TS payload type inside Lifted<...>; preserves a `| undefined` union.
  private def tsPayload(vt: ValueTy): String =
    val core = vt -- UndefT
    val base =
      if core.isBottom then ""
      else if core <= StrT then "string"
      else if core <= MathT || core <= NumberT then "number"
      else if core <= BoolT then "boolean"
      else if core <= BigIntT then "bigint"
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
        case ForOfStmt(elem, iterable, body) =>
          s"for (var $elem of $iterable)" +
          LINE_SEP +
          s"${body.toString(depth)}"
        case ForEachIntStmt(
              index,
              low,
              lowInc,
              high,
              highInc,
              ascending,
              body,
              branchId,
            ) =>
          // Emit the whole integer-loop index sequence as one `range` runtime op
          // (see SpecRuntime.range) driven by `for...of`, instead of desugaring it
          // here into add/subtract/condition. Routing the loop through a single op
          // lets an analysis observe it as a unit; `range` itself re-registers the
          // loop-bound comparison via `condition(branchId, ...)` on each step, so a
          // symbolic `high` (e.g. a string length) stays a flippable path
          // constraint. The index stays a Lifted<number>, so a native `for`
          // counter — which would coerce the proxy and break the value domain — is
          // still avoided; `range` advances it through the runtime `add`/`subtract`.
          val range =
            s"${RUNTIME}.range(($low as Lifted<number>), $lowInc, ($high as Lifted<number>), $highInc, $ascending, Number.MAX_SAFE_INTEGER - $branchId)"
          s"for (var $index of $range)" + LINE_SEP + s"${body.toString(depth)}"
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

  // for (var elem of iterable) { body }
  // Generic for-each over any iterable (arrays, and the `range` index sequence).
  case class ForOfStmt(elem: String, iterable: String, body: Stmt) extends Stmt

  // for (var index = start; index < end; index++) { body }
  // `branchId` keys the loop-bound comparison as a flippable path constraint
  // (the bound is symbolic when `high` carries a Sym, e.g. a symbolic length).
  case class ForEachIntStmt(
    index: String,
    low: String,
    lowInc: Boolean,
    high: String,
    highInc: Boolean,
    ascending: Boolean,
    body: Stmt,
    branchId: Int,
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
