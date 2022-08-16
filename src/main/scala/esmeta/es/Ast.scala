package esmeta.es

import esmeta.ir.Type
import esmeta.es.util.*
import esmeta.spec.*
import esmeta.util.*
import scala.annotation.tailrec

/** abstract syntax tree (AST) values */
sealed trait Ast extends ESElem with Locational {

  /** production names */
  val name: String

  /** parent */
  var parent: Option[Ast] = None

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
  def getChildren(kind: String): List[Ast] = this match
    case lex: Lexical => List()
    case Syntactic(k, _, _, children) =>
      val founded = (for {
        child <- children.flatten
        found <- child.getChildren(kind)
      } yield found).toList
      if (k == kind) this :: founded else founded

  /** types */
  lazy val types: Set[String] =
    Set(name, s"$name$idx") union (this match
      case Syntactic(_, _, _, cs) =>
        (cs match
          case List(Some(child)) => child.types
          case _                 => Set()
        ) + "Nonterminal"
      case _: Lexical => Set("Terminal")
    )

  /** flatten statements */
  // TODO refactoring
  def flattenStmt: List[Ast] = this match
    case Syntactic("Script", _, 0, List(Some(body))) =>
      body match
        case Syntactic("ScriptBody", _, 0, List(Some(stlist))) =>
          flattenStmtList(stlist)
        case _ => Nil
    case _ => Nil

  /** clear location */
  def clearLoc: Ast =
    this match
      case syn: Syntactic =>
        for { child <- syn.children.flatten } child.clearLoc
        syn.loc = None; syn
      case lex: Lexical => lex.loc = None; lex

  /** set location including children */
  def setChildLoc(locOpt: Option[Loc]): Ast = this match
    case syn: Syntactic =>
      for { child <- syn.children.flatten } child.setChildLoc(locOpt)
      syn.loc = locOpt; syn
    case lex: Lexical => lex.loc = locOpt; lex

  /** anot use case class' hash code */
  override def hashCode: Int = super.hashCode
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
