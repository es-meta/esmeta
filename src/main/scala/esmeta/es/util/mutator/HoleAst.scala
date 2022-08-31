package esmeta.es.util.mutator

import esmeta.spec.*
import esmeta.es.*

sealed trait HoleAst extends ESElem {
  val name: String
  var parent: Option[HoleAst] = None

  def astGenerator: Map[String, Ast] => Option[Ast] = inputMap => {
    this match {
      case HoleSyntactic(name, args, rhsIdx, children) =>
        children
          .map(_.map(_.astGenerator(inputMap)))
          .foldRight(Some(List.empty): Option[List[Option[Ast]]]) {
            case (childOptOpt, childrenTemp) =>
              childOptOpt match {
                case Some(childOpt) =>
                  childOpt match {
                    case Some(child) => childrenTemp.map(Some(child) :: _)
                    case None        => None
                  }
                case None => childrenTemp.map(None :: _)
              }
          }
          .map(Syntactic(name, args, rhsIdx, _))
      case HoleLexical(name, str) => Some(Lexical(name, str))
      case Hole(name, args, rhsIdx) =>
        inputMap.get(name).map {
          case s: Syntactic => s.copy(rhsIdx = rhsIdx)
          case l: Lexical   => l
        }
    }
  }
}

object HoleAst:
  def from(ast: Ast): HoleAst = {
    ast match {
      case Syntactic(name, args, rhsIdx, children) =>
        HoleSyntactic(name, args, rhsIdx, children.map(_.map(from)))
      case Lexical(name, str) => HoleLexical(name, str)
    }
  }

/** ASTs constructed by syntactic productions */
case class HoleSyntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: List[Option[HoleAst]],
) extends HoleAst

/** ASTs constructed by lexical productions */
case class HoleLexical(
  name: String,
  str: String,
) extends HoleAst

case class Hole(
  name: String,
  args: List[Option[NtArg]],
  rhsIdx: Int,
) extends HoleAst
