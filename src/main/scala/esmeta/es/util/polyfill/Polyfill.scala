package esmeta.es.util.polyfill

import esmeta.LINE_SEP
import esmeta.spec.*

/** polyfill code
  *
  * @param head
  *   the head of the algorithm this polyfill is compiled from, which tells what
  *   the polyfill is (an abstract operation or a built-in) and where it belongs
  * @param deps
  *   the operations the body calls through unresolved symbols, in order of
  *   first use, which the packager links to their implementations
  */
case class Polyfill(
  head: Head,
  body: Polyfill.Stmt,
  deps: List[Polyfill.Dep] = Nil,
) {
  def name: String = head.fname
  def params: List[Param] = head.originalParams

  override def toString: String = headToString + " " + body.toString

  def headToString: String = {
    import esmeta.spec.ParamKind.*
    val paramStr = params
      .filter(_.kind != Optional)
      .map(param => (if (param.kind == Variadic) "..." else "") + param.name)
      .mkString(", ")
    s"($paramStr)"
  }
}

object Polyfill {

  /** an operation a polyfill calls through an unresolved symbol */
  enum Dep:
    /** an abstract operation of the specification */
    case AO(name: String)

    /** an internal operation, implemented only by the hand-written runtime */
    case Internal(name: String)

    /** a numeric method of a numeric type, e.g. `Number::lessThan` */
    case Numeric(ty: String, name: String)

    /** an enum value, e.g. `~empty~`, obtained from the hand-written runtime */
    case Enum(name: String)

    /** the symbol the generated code calls it by */
    def symbol: String = this match
      case AO(name)          => s"AO__$name"
      case Internal(name)    => s"IN__$name"
      case Numeric(ty, name) => s"${ty}__$name"
      case Enum(name)        => s"ENUM__${name.replaceAll("\\W", "_")}"

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
          val cond = s"$index " + (if (highInc) "<=" else "<") + high
          s"for ($init; $cond; $index++)" + LINE_SEP + s"${body.toString(depth)}"
        case ForEachIntStmt(index, low, lowInc, high, highInc, false, body) =>
          val init = s"var $index = $low" + (if (lowInc) "" else " - 1")
          val cond = s"$index " + (if (highInc) ">=" else ">") + high
          s"for ($init; $cond; $index--)" + LINE_SEP + s"${body.toString(depth)}"
        case BlockStmt(stmts) =>
          "{" + LINE_SEP + stmts
            .map(_.toString(depth + 1))
            .mkString + (TAB * depth) + "}"
        case TryCatchStmt(tryStmt, catchVar, catchStmt) =>
          "try" +
          LINE_SEP +
          tryStmt.toString(depth) +
          (TAB * depth) + s"catch($catchVar)" +
          LINE_SEP + catchStmt.toString(depth)
    } + LINE_SEP

    def toList: List[Stmt] = this match {
      case BlockStmt(stmts) => stmts
      case stmt             => List(stmt)
    }

    def ++(other: Stmt): Stmt = {
      new BlockStmt(this.toList ++ other.toList)
    }
  }

  case class NormalStmt(code: String) extends Stmt

  case class IfStmt(cond: String, thenStmt: Stmt, elseStmt: Option[Stmt])
    extends Stmt

  case class WhileStmt(cond: String, body: Stmt) extends Stmt

  case class ForEachStmt(index: String, end: String, body: Stmt) extends Stmt

  case class ForEachIntStmt(
    index: String,
    low: String,
    lowInc: Boolean,
    high: String,
    highInc: Boolean,
    ascending: Boolean,
    body: Stmt,
  ) extends Stmt

  case class BlockStmt(stmts: List[Stmt]) extends Stmt

  case class TryCatchStmt(tryStmt: Stmt, catchVar: String, catchStmt: Stmt)
    extends Stmt
}
