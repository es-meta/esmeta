package esmeta.solver

import esmeta.cfg.{Branch, CFG}
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** the spec's own vocabulary: what it writes down without being asked */
class AtomDeriver(cfg: CFG) {

  import AtomDeriver.*

  /** spec literals the given type admits */
  def literalsFor(ty: ValueTy): List[String] =
    // a math slot takes the same literal, but no BigDecimal is NaN or infinite
    numbers
      .filter(n =>
        ty.number.contains(n) ||
        (n.double.isFinite && ty.math.contains(Math(n.double))),
      )
      .map(numberLit) ++
    (ty.bigInt match
      case Many => bigInts.map(n => s"${n}n")
      case _    => Nil
    ) ++
    (ty.str match
      case Many => strings.map(s => "\"" + normStr(s) + "\"")
      case _    => Nil
    )

  /** the forms seeded by hand, for what a builtin path states poorly */
  lazy val fromSyntax: List[Template] = List(
    Template(RecordT("OrdinaryObject"), "{}"),
    Template(FunctionT, "() => {}"),
    Template(
      RecordT("ECMAScriptFunctionObject", List("Call", "Construct")),
      "function(){}",
    ),
    Template(SymbolT, "Symbol()"),
    Template(RecordT("Generator"), "(function*(){})()"),
    Template(RecordT("AsyncGenerator"), "(async function*(){})()"),
    Template(
      RecordT("ArgumentsExoticObject"),
      "(function(){ return arguments; })()",
    ),
  )

  /** zero-argument constructions, for types no template reaches */
  lazy val constructions: List[String] = for {
    f <- cfg.funcs
    if TemplateDeriver.usesNewTarget(f)
    surface <- Solver.funcAccessExpr(f).toList
  } yield Solver.newExpr(surface, Nil)

  private lazy val (numbers, bigInts, strings) = collected(for {
    func <- cfg.funcs
    node <- func.nodes.toList
    case branch: Branch <- List(node)
  } yield branch.cond)
}

object AtomDeriver {

  /** JavaScript source for a Number value */
  def numberLit(n: Number): String =
    val d = n.double
    if (d.isNaN) "NaN"
    else if (d.isPosInfinity) "Infinity"
    else if (d.isNegInfinity) "-Infinity"
    else if (d == 0 && 1 / d < 0) "-0"
    else if (d.isWhole && d.abs <= 9007199254740991.0) d.toLong.toString
    else d.toString

  // BigDecimal holds no NaN, no infinity, and collapses -0, so two maps
  private def collected(
    exprs: List[Expr],
  ): (List[Number], List[SBigInt], List[String]) =
    val decimals = MMap[BigDecimal, Int]().withDefaultValue(0)
    val doubles = MMap[Double, Int]().withDefaultValue(0)
    val bigInts = MMap[SBigInt, Int]().withDefaultValue(0)
    val strings = MMap[String, Int]().withDefaultValue(0)
    def bump[T](to: MMap[T, Int], key: T): Unit = to(key) = to(key) + 1
    val walker = new UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case EStr(str)  => bump(strings, str)
        case EBigInt(n) => bump(bigInts, n)
        case ENumber(d) if !d.isFinite || (d == 0 && 1 / d < 0) =>
          bump(doubles, d)
        case _ =>
          folded(expr) match
            case Some(n) => bump(decimals, n)
            case None    => super.walk(expr)
      // a field name is a slot, not a value the spec compares against
      override def walk(ref: Ref): Unit = ref match
        case Field(base, _) => walk(base)
        case _              => super.walk(ref)
    }
    exprs.foreach(walker.walk)
    def ranked[T](from: MMap[T, Int])(using Ordering[T]): List[T] =
      from.toList.sortBy((lit, n) => (-n, lit)).map(_._1)
    val numbers = ranked(decimals).map(n => Number(n.toDouble)) ++
      ranked(doubles).map(Number(_))
    (numbers, ranked(bigInts), ranked(strings))

  // the spec states a size as arithmetic more often than as a number
  private def folded(expr: Expr): Option[BigDecimal] = expr match
    case EMath(n)   => Some(n)
    case ENumber(d) => Option.when(!d.isNaN && !d.isInfinite)(BigDecimal(d))
    case EUnary(UOp.Neg, e) => folded(e).map(-_)
    case EBinary(bop, left, right) =>
      for {
        x <- folded(left)
        y <- folded(right)
        z <- bop match
          case BOp.Add => Some(x + y)
          case BOp.Sub => Some(x - y)
          case BOp.Mul => Some(x * y)
          case BOp.Div => Option.when(y != 0)(x / y)
          // BigDecimal has no negative power, so state it as the division
          case BOp.Pow if y.isValidInt =>
            if (y >= 0) Some(x.pow(y.toInt))
            else Option.unless(x == 0)(BigDecimal(1) / x.pow(-y.toInt))
          case _ => None
      } yield z
    case _ => None
}
