// package esmeta.peval.util

// import esmeta.cfg.{Block, Branch, Call, Node}
// import esmeta.cfgBuilder.CFGBuilder
// import esmeta.ir.*
// import esmeta.ir.util.*
// import esmeta.util.BaseUtils.cached

// import scala.annotation.tailrec
// import scala.collection.mutable.{Set as MSet, Map as MMap}

// /** WIP */
// class UseDef(func: Func) {
//   import UseDef.*

//   lazy val cfg = CFGBuilder(Program(List(func)), log = false)
//   lazy val cfgFunc = cfg.fnameMap(func.name)
//   lazy val nodesWithPos = cfg.nodes
//     .map[Set[(InstCoordinate, Inst)]](_ match
//       case Block(id, insts, _) =>
//         insts.zipWithIndex.map {
//           case (inst, idx) => id -> Some(idx) -> inst
//         }.toSet
//       case Call(id, callInst, _)     => Set(id -> None -> callInst)
//       case Branch(id, _, cond, _, _) => Set(id -> None -> IExpr(cond)),
//     )
//     .reduce(_ | _)

//   lazy val pred = cached { ((nodeId,): InstCoordinate) =>
//     val node = cfg.nodeMap(nodeId)
//     cfg.nodes.filter(n => next(n.id).contains(nodeId)).map(_.id).toSet
//   }

//   lazy val next = cached { (nodeId: InstCoordiate) =>
//     cfg.nodeMap(nodeId) match
//       case Block(id, insts, next)   => next.map(_.id).toSet
//       case Call(id, callInst, next) => next.map(_.id).toSet
//       case Branch(id, kind, cond, thenNode, elseNode) =>
//         thenNode.map(_.id).toSet | elseNode.map(_.id).toSet
//   }

//   lazy val workingSet: MSet[InstCoordinate] = MSet.from(nodesWithPos.map(_._1))

//   lazy private val inOut = MMap.empty[InstCoordinate, (Set[Local], Set[Local])]

//   lazy val fixedInOut: Map[InstCoordinate, (Set[Local], Set[Local])] = {
//     while (!workingSet.isEmpty) {
//       val nodeId = workingSet.head
//       workingSet -= nodeId;
//       val node = cfg.nodeMap(nodeId);
//       val (inOld, outOld) = inOut(nodeId);

//       val inNew = pred(nodeId).map(k => inOut(k)._2).reduce(_ | _)
//       val outNew = genKill(nodeId)._1 | (inNew -- genKill(nodeId)._2)

//       inOut += nodeId -> (inNew -> outNew);

//       if (inNew != inOld || outNew != outOld) {
//         workingSet ++= next(nodeId)
//       }
//     };
//     Map.from(inOut)
//   }

// }

// object UseDef {
//   type InstCoordinate = (Int, Option[Int])
// }
