package esmeta.es.util

import esmeta.es.*
import esmeta.spec.*

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

/** TODO polyfill builder */
case class PolyfillBuilder() {
  import Polyfill.*

  /** create a new scope with a given procedure */
  def newScope(doit: => Unit): Stmt =
    scopes.push((ListBuffer(), ConcurrentHashMap()))
    doit
    BlockStmt(scopes.pop._1.toList)

  /** add JS statements to the current scope */
  def addStmt(stmts: Stmt*): Unit = scopes.head._1 ++= stmts
    .flatMap {
      case BlockStmt(is) => is
      case i             => List(i)
    }

  def currentResult: Stmt = scopes.foldLeft[Stmt](NormalStmt("...")) {
    case (acc, s) => BlockStmt(s._1.toList :+ acc)
  }

  def addVariable(name: String, stmt: Stmt): Stmt = {
    val idx = scopes.head._1.length
    addTag(name, VariableReference(idx, stmt))
    stmt
  }

  def updateVariable(name: String, stmt: Stmt): Stmt = {
    val idx = scopes.head._1.length
    addTag(name, VariableReference(idx, stmt))
    stmt
  }

  def wrapAbrupt(name: String, body: Stmt): Unit =
    wrapTryCatch(name, body, "abrupt")

  def wrapNormal(name: String, body: Stmt): Unit =
    wrapTryCatch(name, body, "normal")

  private def wrapTryCatch(
    name: String,
    body: Stmt,
    completionType: String,
  ): Unit = {
    scopes.head._2
      .computeIfAbsent(
        name,
        { n => throw RuntimeException(s"Unbound variable: $n") },
      )
      .foreach {
        case VariableReference(idx, ref) =>
          val stmt = scopes.head._1(idx) match {
            case WrappedLetStmt(ownName, ownRef, tryBody, catchBody) =>
              if (ownName != name || ownRef != ref)
                throw RuntimeException(
                  s"Wrapper Binding is not equivalent: $ownName and $name",
                );
              else
                completionType match {
                  case "normal" =>
                    WrappedLetStmt(
                      name,
                      ref,
                      CompoundStatement(List(tryBody, body)),
                      catchBody,
                    );
                  case "throw" | "abrupt" =>
                    WrappedLetStmt(
                      name,
                      ref,
                      tryBody,
                      CompoundStatement(List(catchBody, body)),
                    );
                }
            case _ =>
              completionType match {
                case "normal" =>
                  WrappedLetStmt(
                    name,
                    ref,
                    CompoundStatement(List(body)),
                    NoOpStmt(),
                  );
                case "throw" | "abrupt" =>
                  WrappedLetStmt(
                    name,
                    ref,
                    NoOpStmt(),
                    CompoundStatement(List(body)),
                  );
              }
          }
          scopes.head._1.update(idx, stmt)
        case Enum(_) =>
      }
  }

  def annotate(name: String, value: String): Unit = {
    addTag(name, Enum(value))
  }

  def hasTag(name: String, value: String): Boolean =
    scopes
      .flatMap { it => Option(it._2.get(name)) }
      .exists { list =>
        list.exists {
          case Enum(tagValue) => value == tagValue
          case _              => false
        }
      }

  def tagExists(name: String): Boolean =
    scopes.head._2.containsKey(name) && scopes.head._2.get(name).nonEmpty

  def deleteTag(name: String, tag: String): Unit = scopes.head._2.put(
    name,
    scopes.head._2.get(name).filterInPlace {
      case Enum(value) => value.eq(tag)
      case _           => true
    },
  )

  def getTags(name: String): List[String] = scopes.head._2
    .get(name)
    .flatMap {
      case Enum(tagValue) => Option(tagValue)
      case _              => None
    }
    .toList

  private def addTag(name: String, value: Tag): Unit = {
    val buffer = scopes.head._2.computeIfAbsent(name, _ => ListBuffer[Tag]())
    buffer.addOne(value)
  }

  /** get next temporal variable */
  def newTId: String = s"_x$nextTId"

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // temporal variable index counter
  private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
  private var tidCount: Int = 0

  private sealed trait Tag
  private case class VariableReference(idx: Int, ref: Stmt) extends Tag
  private case class Enum(value: String) extends Tag

  /** scope stacks */
  private var scopes
    : Stack[(ListBuffer[Stmt], ConcurrentHashMap[String, ListBuffer[Tag]])] =
    Stack()
}
