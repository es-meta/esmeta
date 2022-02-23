package esmeta.js

import esmeta.js.util.*
import esmeta.spec.*
import scala.annotation.tailrec

/** abstract syntax tree (AST) values */
sealed trait Ast extends JSElem {

  /** production names */
  val name: String

  /** idx of production */
  def idx: Int = this match
    case lex: Lexical               => 0
    case Syntactic(_, _, rhsIdx, _) => rhsIdx

  /** production chains */
  def chains: List[Ast]

  // /** equality */
  // override def hashCode: Int = super.hashCode
  // override def equals(any: Any): Boolean = any match
  //   case that: Ast => this eq that
  //   case _         => false

  /** flatten statements */
  // TODO refactoring
  def flattenStmt: List[Ast] = this match
    case Syntactic("Script", _, 0, List(Some(body))) =>
      body match
        case Syntactic("ScriptBody", _, 0, List(Some(stlist))) =>
          flattenStmtList(stlist)
        case _ => Nil
    case _ => Nil
}

/** ASTs constructed by syntatic productions */
case class Syntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: List[Option[Ast]],
) extends Ast {

  /** chain productions */
  override lazy val chains: List[Ast] =
    children.flatten match
      case child :: Nil => this :: child.chains
      case _            => List(this)
}

/** ASTs constructed by lexical productions */
case class Lexical(
  name: String,
  str: String,
) extends Ast {
  override def chains: List[Ast] = List(this)
}
