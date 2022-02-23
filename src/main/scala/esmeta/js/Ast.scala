package esmeta.js

import esmeta.ir.Type
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
  lazy val chains: List[Ast] = this match
    case lex: Lexical => List(this)
    case syn: Syntactic =>
      syn.children.flatten match
        case child :: Nil => this :: child.chains
        case _            => List(this)

  /** children */
  def getChildren: List[Option[Ast]] = this match
    case lex: Lexical   => List()
    case syn: Syntactic => syn.children

  /** type of */
  // TODO type
  def typeCheck(ty: Type): Boolean = {
    val tname = ty.name
    if (tname.startsWith("|") && tname.endsWith("|")) {
      val ntName = tname.substring(1, tname.length - 1)
      ntName == name // TODO handle child instance
    } else ???
  }

  // TODO tweak equality for fast caching
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
) extends Ast

/** ASTs constructed by lexical productions */
case class Lexical(
  name: String,
  str: String,
) extends Ast
