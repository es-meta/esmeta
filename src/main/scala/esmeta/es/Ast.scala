package esmeta.es

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.util.*
import esmeta.ir.Type
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import scala.annotation.tailrec

/** abstract syntax tree (AST) values */
sealed trait Ast extends ESElem with Locational {

  /** production names */
  val name: String

  /** parent */
  var parent: Option[Ast] = None

  /** children */
  def children: Vector[Option[Ast]]

  /** idx of production */
  def idx: Int = this match
    case lex: Lexical               => 0
    case Syntactic(_, _, rhsIdx, _) => rhsIdx
    case h: Hole                    => ???

  /** validity check */
  def valid(grammar: Grammar): Boolean = AstValidityChecker(grammar, this)

  /** get arguments */
  def getArgs: List[Boolean] = this match
    case lex: Lexical   => Nil
    case syn: Syntactic => syn.args
    case _: Hole        => ???

  /** production chains */
  lazy val chains: List[Ast] = this :: (this match
    case lex: Lexical => Nil
    case syn: Syntactic =>
      syn.children.flatten match
        case Vector(child) => this :: child.chains
        case _             => List(this)
    case _: Hole => ???
  )

  /** types */
  lazy val types: Set[String] = (this match
    case _: Lexical => Set()
    case syn: Syntactic =>
      syn.children.flatten match
        case Vector(child) => child.types
        case _             => Set()
    case _: Hole => ???
  ) + name

  /** flatten statements */
  def flattenStmt: Vector[Ast] = this match
    case Syntactic("Script", _, 0, Vector(Some(body))) =>
      body match
        case Syntactic("ScriptBody", _, 0, Vector(Some(stlist))) =>
          flattenStmtList(stlist)
        case _ => Vector.empty
    case _ => Vector.empty

  /** clear location */
  def clearLoc: Ast =
    this match
      case syn: Syntactic =>
        for { child <- syn.children.flatten } child.clearLoc
        syn.loc = None; syn
      case lex: Lexical => lex.loc = None; lex
      case hole: Hole   => hole.loc = None; hole

  /** set location including children */
  def setChildLoc(locOpt: Option[Loc]): Ast = this match
    case syn: Syntactic =>
      for { child <- syn.children.flatten } child.setChildLoc(locOpt)
      syn.loc = locOpt; syn
    case lex: Lexical => lex.loc = locOpt; lex
    case hole: Hole   => hole.loc = locOpt; hole

  /** safe getter */
  def get(field: Value)(using cfg: CFG): Option[Ast] = (this, field) match
    case (_, Str("parent")) => parent
    // TODO remove this case if possible
    case (syn: Syntactic, Str(fieldStr)) =>
      val Syntactic(name, _, rhsIdx, children) = syn
      val rhs = cfg.grammar.nameMap(name).rhsVec(rhsIdx)
      rhs.getRhsIndex(fieldStr).flatMap(children(_))
    case (syn: Syntactic, Math(n)) if n.isValidInt =>
      syn.children(n.toInt)
    case _ => None

  /** getter */
  def apply(field: Value)(using cfg: CFG): Ast =
    get(field).getOrElse(throw InvalidAstField(this, field))

  /** existence check */
  def exists(field: Value)(using cfg: CFG): Boolean = get(field).isDefined

  /** get syntax-directed operation (SDO) */
  def getSdo(name: String)(using cfg: CFG): Option[(Ast, Func)] =
    val fnameMap = cfg.fnameMap
    chains.foldLeft[Option[(Ast, Func)]](None) {
      case (None, ast0) =>
        val subIdx = ast0.subIdx
        val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$name"
        fnameMap
          .get(fname)
          .orElse(fnameMap.get(s"<DEFAULT>.$name"))
          .map((ast0, _))
      case (res: Some[_], _) => res
    }

  /** get sub index of parsed Ast */
  def subIdx(using cfg: CFG): Int = this match
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      cfg.grammar.nameMap.get(name).fold(0) { prod =>
        val rhs = prod.rhsVec(rhsIdx)
        val optionals = (for {
          ((_, opt), child) <- rhs.ntsWithOptional zip children if opt
        } yield !child.isEmpty)
        optionals.reverse.zipWithIndex.foldLeft(0) {
          case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
          case (acc, _)           => acc
        }
      }
    case _: Hole => ???

  /** not use case class' hash code */
  override def hashCode: Int = super.hashCode
}

case class Hole(
  name: String,
  args: List[Boolean],
  label: String, // e1 …
  attrs: Map[String, String], // key -> value
) extends Ast {
  def children: Vector[Option[Ast]] = Vector.empty
}

/** ASTs constructed by syntactic productions */
case class Syntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: Vector[Option[Ast]],
) extends Ast

/** ASTs constructed by lexical productions */
case class Lexical(
  name: String,
  str: String,
) extends Ast {
  def children: Vector[Option[Ast]] = Vector.empty
}
