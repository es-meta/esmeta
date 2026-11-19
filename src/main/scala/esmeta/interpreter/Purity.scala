package esmeta.interpreter

import esmeta.cfg.{CFG, Func}
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.state.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** purity checker for functions in a CFG
  *
  * A function is pure if it does not touch global variables, allocate objects,
  * modify the heap, use nondeterministic or effectful expressions, or call
  * functions other than pure ones known statically. When such a function is
  * called with plain arguments (not addresses or callables), it cannot reach
  * the heap at all, so its result depends only on its arguments and the
  * interpreter may cache it without changing the semantics.
  */
class Purity(cfg: CFG) {

  /** check whether a function is pure */
  def apply(func: Func): Boolean = cache.get(func.id) match
    case Some(pure) => pure
    case None =>
      visiting += func.id
      val checker = Checker()
      checker.walk(func.irFunc.body)
      visiting -= func.id
      cache += func.id -> checker.pure
      checker.pure

  /** check whether results of a pure function call with given arguments are
    * worth caching: all arguments are plain values, which cannot reach the
    * heap, and some of them are ASTs, which are immutable and hashed by
    * identity, while calls with only primitive values are cheaper to compute
    * than to cache
    */
  def isCacheable(args: List[Value]): Boolean =
    args.forall(isPlain) && args.exists(_.isInstanceOf[AstValue])

  /** check whether a value is plain, which cannot reach the heap */
  def isPlain(value: Value): Boolean = value match
    case _: Addr | _: Callable => false
    case _                     => true

  private val cache: MMap[Int, Boolean] = MMap()

  // functions being checked, considered impure for recursive calls
  private val visiting: MSet[Int] = MSet()

  private class Checker extends UnitWalker {
    var pure: Boolean = true

    override def walk(inst: Inst): Unit = inst match
      case IAssign(_: Field | _: Global, _) | IExpand(_, _) | IDelete(_, _) |
          IPush(_, _, _) | IPop(_, _, _) | IPrint(_) | ISdoCall(_, _, _, _) =>
        pure = false
      case ICall(_, EClo(fname, Nil), args) =>
        val callee = cfg.fnameMap.get(fname)
        if (!callee.exists(f => !visiting.contains(f.id) && apply(f)))
          pure = false
        walkList(args, walk)
      case ICall(_, _, _) => pure = false
      case _              => super.walk(inst)

    override def walk(expr: Expr): Unit = expr match
      case _: EParse | _: EYet | _: ERandom | _: EDebug | _: ECont |
          _: AllocExpr =>
        pure = false
      case EClo(_, captured) if captured.nonEmpty => pure = false
      case _                                      => super.walk(expr)

    override def walk(x: Var): Unit = x match
      case _: Global => pure = false
      case _         =>
  }
}
