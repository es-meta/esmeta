package esmeta.state

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.util.Loc

/** Target syntax Information */
case class Target(prodName: String, rhsIdx: Int, subIdx: Int, loc: Loc)

object Target {

  /** create Target from AST */
  def apply(ast: Option[Ast])(using cfg: CFG): Option[Target] = for {
    case ast @ Syntactic(name, _, idx, _) <- ast
    subIdx = ast.subIdx
    loc <- ast.loc
  } yield Target(name, idx, subIdx, loc)
}
