package esmeta.peval.util

import esmeta.cfg.{Block, Branch, Call, Node}
import esmeta.cfgBuilder.CFGBuilder
import esmeta.ir.*
import esmeta.ir.util.*
import esmeta.util.BaseUtils.cached

import scala.annotation.tailrec
import scala.collection.mutable.{Set as MSet, Map as MMap}

/** WIP */
class GenKill(func: Func) {
  import GenKill.*

  lazy val cfg = CFGBuilder(Program(List(func)), log = false)
  lazy val cfgFunc = cfg.fnameMap(func.name)
  lazy val definitions: Map[Local, Set[Definition]] = cfgFunc.nodes
    .flatMap((node) => {
      node match
        case Block(_, insts, _) =>
          List
            .from[NormalInst](insts)
            .zipWithIndex
            .map[Set[Definition]] {
              case (inst, i) =>
                inst match
                  case ILet(lhs, expr) =>
                    Set(Definition(lhs, node.id -> Some(i)))
                  case IAssign(lhs: Local, expr) =>
                    Set(Definition(lhs, node.id -> Some(i)))
                  case IPop(lhs, list, front) =>
                    Set(Definition(lhs, node.id -> Some(i)))
                  case _ => Set.empty[Definition]
            }
            .reduce[Set[Definition]](_ | _)
        case Call(id, callInst, _) =>
          Set(Definition(callInst.lhs, node.id -> None))
        case _: Branch => Set.empty
    })
    .groupMap[Local, Definition] { case Definition(local, _) => local } {
      case x => x
    }

  lazy val pred = cached { (nodeId: Int) =>
    val node = cfg.nodeMap(nodeId)
    cfg.nodes.filter(n => next(n.id).contains(nodeId)).map(_.id).toSet
  }

  lazy val next = cached { (nodeId: Int) =>
    cfg.nodeMap(nodeId) match
      case Block(id, insts, next)   => next.map(_.id).toSet
      case Call(id, callInst, next) => next.map(_.id).toSet
      case Branch(id, kind, cond, thenNode, elseNode) =>
        thenNode.map(_.id).toSet | elseNode.map(_.id).toSet
  }

  lazy val workingSet: MSet[Int] = MSet.from(cfg.nodes.map(_.id))

  lazy val inOut = MMap.empty[Int, (Set[Definition], Set[Definition])]

  lazy val reduced: Func = {
    while (!workingSet.isEmpty) {
      val nodeId = workingSet.head
      workingSet -= nodeId;
      val node = cfg.nodeMap(nodeId);
      val (inOld, outOld) = inOut(nodeId);

      val inNew = pred(nodeId).map(k => inOut(k)._2).reduce(_ | _)
      val outNew = genKill(nodeId)._1 | (inNew -- genKill(nodeId)._2)

      inOut += nodeId -> (inNew -> outNew);

      if (inNew != inOld || outNew != outOld) {
        workingSet ++= next(nodeId)
      }

    };
    ???
  }

  private val genKill = cached { (nodeId: Int) =>
    val node = cfg.nodeMap(nodeId)
    node match
      case Block(_, insts, next) =>
        insts.zipWithIndex.foldLeft(
          Set.empty[Definition] -> Set.empty[Definition],
        ) {
          case (gen -> kill, inst -> i) =>
            val thisGen -> thisKill =
              genKillNormalInst(inst, node.id -> Some(i))
            (gen -- thisKill ++ thisGen) -> (kill | thisKill)
        }
        val size = insts.size
        val genset = insts.zipWithIndex
          .map((inst, i) => {
            // genInst(i) - killInst(i+1) ... - killInst(k)
            val gen_i =
              (genKillNormalInst((inst, (node.id -> Some(i))))._1, inst, i)._1
            val kill_js = (i + 1 to size - 1).map((idx) => {
              genKillNormalInst(
                insts(idx) -> (node.id -> Some(idx)),
              )._2 // killInst(.)
            })
            kill_js.foldLeft[Set[Definition]](gen_i)(_.diff(_))
          })
          .reduce(_ | _)
        val killset =
          insts.zipWithIndex
            .map((inst, i) =>
              genKillNormalInst((inst, (node.id -> Some(i))))._2,
            )
            .reduce[Set[Definition]](_ | _)
        (genset, killset)
      case Call(_, callInst, next) =>
        genKillCallInst((callInst, (node.id -> None)))
      case _ => (Set.empty, Set.empty)
  }

  private val genKillNormalInst = cached[
    (NormalInst, (Int, Option[Int])),
    (Set[Definition], Set[Definition]),
  ] {
    case (inst: NormalInst, (nodeId: Int, idx: Option[Int])) => {
      val at = (nodeId, idx)
      inst match
        case IAssign(lhs: Local, _) =>
          val gen = Definition(lhs, at)
          Set(gen) -> (definitions.get(lhs).getOrElse(Set.empty) - gen)
        case ILet(lhs, _) =>
          val gen = Definition(lhs, at)
          Set(gen) -> (definitions.get(lhs).getOrElse(Set.empty) - gen)
        case IPop(lhs, _, _) =>
          val gen = Definition(lhs, at)
          Set(gen) -> (definitions.get(lhs).getOrElse(Set.empty) - gen)
        case _ => (Set.empty, Set.empty)
    }
  }

  private val genKillCallInst = cached[
    (CallInst, (Int, Option[Nothing])),
    (Set[Definition], Set[Definition]),
  ] {
    case (inst: CallInst, at: (Int, Option[Nothing])) => {
      val gen = Definition(inst.lhs, at)
      Set(gen) -> (definitions.get(inst.lhs).getOrElse(Set.empty) - gen)
    }
  }

}

object GenKill {

  extension (inst: Inst) {
    def gen = inst match
      case IExpr(expr)                   =>
      case ILet(lhs, expr)               =>
      case IAssign(ref, expr)            =>
      case IExpand(base, expr)           =>
      case IDelete(base, expr)           =>
      case IPush(elem, list, front)      =>
      case IPop(lhs, list, front)        =>
      case IReturn(expr)                 =>
      case IAssert(expr)                 =>
      case IPrint(expr)                  =>
      case INop()                        =>
      case IIf(cond, thenInst, elseInst) =>
      case IWhile(cond, body)            =>
      case ICall(lhs, fexpr, args)       =>
      case ISdoCall(lhs, base, op, args) =>
      case ISeq(insts)                   =>

  }

  private def memoized[K, V](cache: MMap[K, V], key: K)(value: => V): V =
    cache.get(key) match
      case None       => cache += key -> value; value
      case Some(memo) => memo

  @tailrec
  private def fix[A](f: A => A, x: A): A =
    val fx = f(x)
    if (fx == x) then fx else fix(f, fx)

  /** @param local
    * @param nodeId
    * @param nth
    */
  case class Definition(local: Local, at: (Int, Option[Int]))

}
