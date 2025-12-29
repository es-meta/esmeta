package esmeta.es

import esmeta.LINE_SEP
import esmeta.spec.*

/** polyfill code */
case class Polyfill(
  name: String,
  params: List[Param],
  body: Polyfill.Stmt,
) {
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
        case BlockStmt(stmts) =>
          "{" + LINE_SEP + stmts
            .map(_.toString(depth + 1))
            .mkString + (TAB * depth) + "}"
    } + LINE_SEP

    def toList: List[Stmt] = this match {
      case BlockStmt(stmts) => stmts
      case stmt             => List(stmt)
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

  // { stmts }
  case class BlockStmt(stmts: List[Stmt]) extends Stmt
}
