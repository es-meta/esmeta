package esmeta.opcount

import esmeta.es.util.PolyfillGenerator
import esmeta.lang.*
import esmeta.lang.util.UnitWalker
import esmeta.util.ManualInfo
import scala.collection.mutable.{Map => MMap}

/** Count the places where `PolyfillGenerator` emits `$.default` for an
  * algorithm, i.e., the operations that get no Table I hook.
  *
  * Each site is classified as
  *   - `const:<kind>`: a value without operands (`$.default(v, [])`), such as a
  *     literal; its shadow value is determined by the concrete value alone
  *   - `op:<name>`: a value computed natively from operands and wrapped by
  *     `$.default(v, [operands])`; the analysis sees only the default transfer
  *
  * The cases mirror `PolyfillGenerator.compile`: literals, `NumberOfExpression`
  * (List length), `IntrinsicExpression`, `AbstractClosureExpression`,
  * `NewTargetLiteral`, boxed-primitive data slots, and the `polyfill-rule.json`
  * snippets that unparsed steps (`YetStep`/`YetExpression`) compile to.
  */
class DefaultCounter extends UnitWalker {
  val sites: MMap[String, Int] = MMap().withDefaultValue(0)
  private def add(k: String): Unit = sites(k) += 1

  /** whether some `$.default(v, [ops])` call in a snippet has non-empty `ops`:
    * scan each call's parenthesized arguments, skipping string literals, and
    * inspect its last top-level argument */
  private def hasOperands(code: String): Boolean =
    val call = """\$\.default(<[^>]*>)?\(""".r
    call.findAllMatchIn(code).exists { m =>
      var (i, depth, lastComma) = (m.end, 1, -1)
      var quote: Char = 0
      while (i < code.length && depth > 0) {
        val c = code(i)
        if (quote != 0) { if (c == '\\') i += 1 else if (c == quote) quote = 0 }
        else c match
          case '"' | '\'' | '`'   => quote = c
          case '(' | '[' | '{'    => depth += 1
          case ')' | ']' | '}'    => depth -= 1
          case ',' if depth == 1  => lastComma = i
          case _                  =>
        i += 1
      }
      lastComma >= 0 && {
        val last = code.substring(lastComma + 1, i - 1).trim
        last.startsWith("[") && last.stripPrefix("[").stripSuffix("]").trim.nonEmpty
      }
    }

  private def yet(str: String): Unit =
    ManualInfo.polyfillRule.get(str) match
      case Some(code) if hasOperands(code) =>
        add(s"op:rule:${str.take(60)}")
      case Some(code) if code.contains("$.default") => add("const:rule-literal")
      case Some(_)                                  =>
      case None => add(s"unhandled:${str.take(60)}")

  override def walk(step: Step): Unit = step match
    case _: AssertStep => // not compiled
    case YetStep(YetExpression(str, block)) =>
      yet(str); block.foreach(walk)
    case _ => super.walk(step)

  override def walk(expr: Expression): Unit =
    expr match
      case YetExpression(str, block) => yet(str)
      case NumberOfExpression(_, _, ReferenceExpression(_), _) =>
        add("op:List length")
      case _: IntrinsicExpression       => add("const:intrinsic")
      case _: AbstractClosureExpression => add("const:abstract closure")
      case _                            =>
    expr match
      case YetExpression(_, block) => block.foreach(walk)
      case _                       => super.walk(expr)

  override def walk(ref: Reference): Unit =
    ref match
      case Access(_, name, AccessKind.Field, _)
          if PolyfillGenerator.boxedSlotCtor.contains(name) =>
        add(s"op:[[$name]] read")
      case _ =>
    super.walk(ref)

  override def walk(lit: Literal): Unit = lit match
    case _: NewTargetLiteral => add("const:new.target")
    case _: HexLiteral | _: CodeLiteral | _: EnumLiteral | _: StringLiteral |
        _: SymbolLiteral | _: PositiveInfinityMathValueLiteral |
        _: NegativeInfinityMathValueLiteral | _: DecimalMathValueLiteral |
        _: NumberLiteral | _: BigIntLiteral | _: TrueLiteral | _: FalseLiteral |
        _: UndefinedLiteral | _: NullLiteral =>
      add("const:literal")
    case _ =>
}
