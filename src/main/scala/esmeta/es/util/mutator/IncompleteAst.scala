package esmeta.es.util.mutator

import esmeta.es.util.mutator
import esmeta.spec.*
import esmeta.es.{Ast, ESElem}

sealed trait IncompleteAst extends ESElem {
  val name: String
  var parent: Option[IncompleteAst] = None

  def holeAstGenerator: Map[String, Ast] => Option[HoleAst] = inputMap => {
    this match {
      case IncSyntactic(name, args, rhsIdx, children) =>
        children
          .map(_.map(_.holeAstGenerator(inputMap)))
          .foldRight(Some(List.empty): Option[List[Option[HoleAst]]]) {
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
          .map(HoleSyntactic(name, args, rhsIdx, _))
      case IncLexical(name, str)          => Some(HoleLexical(name, str))
      case IncDesired(name, args, rhsIdx) => Some(Hole(name, args, rhsIdx))
      case IncUndesired(name, args, rhsIdx) =>
        inputMap.get(name).map(HoleAst.from).map {
          case hs: HoleSyntactic => hs.copy(rhsIdx = rhsIdx)
          case x                 => x
        }
    }
  }
}

case class IncSyntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: List[Option[IncompleteAst]],
) extends IncompleteAst

case class IncLexical(
  name: String,
  str: String,
) extends IncompleteAst

case class IncDesired(
  name: String,
  args: List[Option[NonterminalArgument]],
  rhsIdx: Int,
) extends IncompleteAst

case class IncUndesired(
  name: String,
  args: List[Option[NonterminalArgument]],
  rhsIdx: Int,
) extends IncompleteAst
