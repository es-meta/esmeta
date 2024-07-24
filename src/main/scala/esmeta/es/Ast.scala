package esmeta.es

import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.error.InvalidASTItem
import esmeta.ir.Type
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

  /** sub index */
  def subIdx: Int = this match
    case lex: Lexical => 0
    case Syntactic(_, _, _, children) =>
      children.map(child => if (child.isDefined) 1 else 0).fold(0)(_ * 2 + _)

  /** validity check */
  def valid(grammar: Grammar): Boolean = AstValidityChecker(grammar, this)

  /** size */
  lazy val size: Int = this match
    case lex: Lexical => 1
    case syn: Syntactic =>
      syn.children.map(_.fold(1)(_.size)).foldLeft(1)(_ + _)

  /** production chains */
  lazy val chains: List[Ast] = this match
    case lex: Lexical => List(this)
    case syn: Syntactic =>
      syn.children.flatten match
        case child :: Nil => this :: child.chains
        case _            => List(this)

  /** get items */
  def getItems(name: String): List[Ast] = this match
    case _: Lexical => Nil
    case syn: Syntactic =>
      for {
        child <- syn.children.flatten
        item <-
          if (child.name == this.name) child.getItems(name)
          else if (child.name == name) List(child)
          else throw InvalidASTItem(child, name)
      } yield item

  /** types */
  lazy val types: Set[String] =
    Set(name, s"$name$idx") union (this match
      case Syntactic(_, _, _, cs) =>
        (cs match
          case List(Some(child)) => child.types
          case _                 => Set()
        ) + "Nonterminal"
      case _: Lexical => Set()
    ) + "ParseNode"

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

  /** get syntax-directed operation (SDO) */
  def getSdo(name: String)(using cfg: CFG): Option[(Ast, Func)] =
    val fnameMap = cfg.fnameMap
    chains.foldLeft[Option[(Ast, Func)]](None) {
      case (None, ast0) =>
        val subIdx = ast0.getSubIdx
        val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$name"
        fnameMap
          .get(fname)
          .orElse(fnameMap.get(s"<DEFAULT>.$name"))
          .map((ast0, _))
      case (res: Some[_], _) => res
    }

  /** get sub index of parsed Ast */
  def getSubIdx(using cfg: CFG): Int = this match
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        ((_, opt), child) <- rhs.ntsWithOptional zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }

  /** not use case class' hash code */
  override def hashCode: Int = super.hashCode
}

/** ASTs constructed by syntactic productions */
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
