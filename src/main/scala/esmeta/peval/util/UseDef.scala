package esmeta.peval.util

import esmeta.cfg.{Node}
import esmeta.ir.*
import esmeta.ir.util.*
import esmeta.cfgBuilder.CFGBuilder
import scala.annotation.tailrec
import scala.collection.mutable.{Set as MSet, Map as MMap}
import esmeta.cfg.Block
import esmeta.cfg.Call
import esmeta.cfg.Branch

/** WIP */
class UseDef(func: Func) {
  import UseDef.*

  lazy val cfg = CFGBuilder(Program(List(func)), log = false)
  lazy val cfgFunc = cfg.fnameMap(func.name)
  lazy val definitions : Map[Local, Set[Definition]] = cfgFunc.nodes.flatMap((node) => {
    node match
      case Block(id, insts, _) => List.from[NormalInst](insts).zipWithIndex.map[Set[Definition]]{ case (inst, i) => inst match
          case ILet(lhs, expr) => Set(Definition(lhs, id -> Some(i)))
          case IAssign(lhs : Local, expr) => Set(Definition(lhs, id -> Some(i)))
          case IPop(lhs, list, front) =>Set(Definition(lhs, id -> Some(i)))
          case _ => Set.empty[Definition]
      }.reduce[Set[Definition]](_ | _)
      case Call(id, callInst, _) => Set(Definition(callInst.lhs, id -> None))
      case _ : Branch => Set.empty
  }).groupMap[Local, Definition]{ case Definition(local, _) => local }{ case x => x }

  // private lazy val workingSet = Set(cfgFunc.entry.id)
  private lazy val map = Map.empty[Int, GenKill]

  lazy val reduced: Func = {
    // while (!workingSet.isEmpty) {
    //   ???
    // };
    ???
  }


  extension (node: Node) {

    def gen = memoized[Node, Set[Definition]](cache = genMap, key = node) {
      ???
    }

    def kill = memoized[Node, Set[Definition]](cache = killMap, key = node) {
      node match
        case Block(id, _, _)   => Set.empty
        case Call(id, callInst, next) => Set(Definition(callInst.lhs, id -> None))
        case Branch(_, _, _, _, _)    => Set.empty
    }

    def in = ???
    def out = ???
  }

  private def genMap = MMap[Node, Set[Definition]]()
  private def killMap = MMap[Node, Set[Definition]]()
}

object UseDef {

  case class GenKill(var gen: Set[Local], var kill: Set[Local])


  // extension (expr: Expr) {
  //   object Hi {
  //     def apply() : Unit = ()
  //   }
  // }

  // class FindBoundVars extends UnitWalker {
  //   lazy val get: Set[Var] = ???

  //   val vars = MSet.empty[Var]

  //   override def walk(expr: Expr): Unit = ???

  // }

  // private def findBoundNames(expr : Expr): Set[Var] = expr

  extension (inst: Inst) {
    def gen = inst match
      case IExpr(expr) => 
      case ILet(lhs, expr) =>
      case IAssign(ref, expr) =>
      case IExpand(base, expr) =>
      case IDelete(base, expr) =>
      case IPush(elem, list, front) =>
      case IPop(lhs, list, front) =>
      case IReturn(expr) =>
      case IAssert(expr) =>
      case IPrint(expr) =>
      case INop() =>
      case IIf(cond, thenInst, elseInst) =>
      case IWhile(cond, body) =>
      case ICall(lhs, fexpr, args) =>
      case ISdoCall(lhs, base, op, args) =>
      case ISeq(insts) =>

  }
  
  

  private def memoized[K, V](cache: MMap[K, V], key: K)(value: => V): V =
    cache.get(key) match
      case None       => cache += key -> value; value
      case Some(memo) => memo

  @tailrec
  private def fix[A](f: A => A, x: A): A =
    val fx = f(x)
    if (fx == x) then fx else fix(f, fx)


}
