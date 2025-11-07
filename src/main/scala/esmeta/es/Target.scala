package esmeta.es

import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.util.Loc
import esmeta.util.BaseUtils.*

/** Target Information */
enum Target {
  case Normal(loc: Loc)
  case BuiltinThis(thisArg: String)
  case BuiltinArg(arg: String, idx: Int)

  /** extract argument string from Builtin */
  def argStr: String = this match
    case BuiltinThis(thisArg) => thisArg
    case BuiltinArg(arg, _)   => arg
    case _                    => raise("target must be builtin")
}

object Target {
  import Target.*
  import Code.*

  /** create Target from AST */
  def apply(ast: Option[Ast])(using cfg: CFG): Option[Target] = for {
    case ast: Syntactic <- ast
    loc <- ast.loc
  } yield Normal(loc)
}
