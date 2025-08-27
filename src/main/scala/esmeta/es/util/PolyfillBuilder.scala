package esmeta.es.util

import esmeta.es.*
import esmeta.spec.*
import scala.collection.mutable.{ListBuffer, Stack}

/** TODO polyfill builder */
case class PolyfillBuilder(
  spec: Spec,
  algo: Algorithm,
) {
  import Polyfill.*

  /** create a new scope with a given procedure */
  def newScope(doit: => Unit): Stmt =
    scopes.push(ListBuffer())
    doit
    BlockStmt(scopes.pop.toList)

  /** add JS statements to the current scope */
  def addStmt(stmts: Stmt*): Unit = scopes.head ++= stmts
    .flatMap {
      case BlockStmt(is) => is
      case i             => List(i)
    }

  def currentResult: Stmt = scopes.foldLeft[Stmt](NormalStmt("...")) {
    case (acc, s) => BlockStmt(s.toList :+ acc)
  }

  /** get next temporal variable */
  def newTId: String = s"_x$nextTId"

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // temporal variable index counter
  private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
  private var tidCount: Int = 0

  /** scope stacks */
  private var scopes: Stack[ListBuffer[Stmt]] = Stack()
}
