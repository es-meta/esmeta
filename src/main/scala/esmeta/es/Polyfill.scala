package esmeta.es

import esmeta.LINE_SEP

/** polyfill code */
case class Polyfill(
  name: String,
  body: Polyfill.Stmt,
) {
  override def toString: String = body.toString
}

object Polyfill {
  sealed trait Stmt {
    override def toString: String = toString(0)

    private val TAB = "  "
    def toString(depth: Int): String = (TAB * depth) + {
      this match
        case IfStmt(cond, stmt) => s"if ($cond) $stmt"
        case NormalStmt(code)   => code
        case BlockStmt(stmts) =>
          "{" + LINE_SEP +
          stmts.map(_.toString(depth + 1)).mkString +
          "}" + LINE_SEP
    } + LINE_SEP
  }

  // if (cond) { stmt }
  case class IfStmt(cond: String, stmt: Stmt) extends Stmt

  // code
  case class NormalStmt(code: String) extends Stmt

  // { stmts }
  case class BlockStmt(stmts: List[Stmt]) extends Stmt
}
