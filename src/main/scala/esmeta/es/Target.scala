package esmeta.es

import esmeta.cfg.CFG
import esmeta.parser.AstFrom
import esmeta.state.Nearest
import esmeta.util.Loc

/** Target Information */
enum Target:
  case Normal(loc: Loc)
  case BuiltinThis(ast: Ast)
  case BuiltinArg(ast: Ast, idx: Int)

object Target {
  import Target.*
  import Code.*

  /** create Target from AST */
  def apply(ast: Option[Ast])(using cfg: CFG): Option[Target] = for {
    case ast: Syntactic <- ast
    loc <- ast.loc
  } yield Normal(loc)

  /** create list of Target from Builtin */
  def apply(builtin: Code.Builtin)(using
    assignExprParser: AstFrom,
  ): List[Target] =
    val args = for {
      (arg, idx) <- builtin.args.getOrElse(Nil).zipWithIndex
      ast = assignExprParser.from(arg)
    } yield BuiltinArg(ast, idx)
    builtin.thisArg.fold(args)(arg =>
      BuiltinThis(assignExprParser.from(arg)) :: args,
    )
}
