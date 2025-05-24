package esmeta.dump

import esmeta.DUMP_VISUALIZER_LOG_DIR
import esmeta.cfg.*
import esmeta.ir.{Func as IRFunc, Type as IRType, *}
import esmeta.ty.CompT
import esmeta.util.{given_Ordering_Pos, *}
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map as MMap, Set as MSet}

object DumpStepToNodeId {
  def apply(cfg: CFG): Unit = {
    for {
      func <- cfg.funcs
      algo <- func.irFunc.algo
    } {
      val stepToNodeId: MMap[String, MSet[Int]] = MMap.empty
      val abruptMap: MMap[String, MMap[Pos, Int]] = MMap.empty
      walk(func.entry, None, stepToNodeId, abruptMap)

      abruptMap.foreach {
        case (step, posSet) =>
          posSet.toSeq
            .sorted(Ordering.by[(Pos, Int), Pos](_._1))
            .zipWithIndex
            .foreach {
              case ((pos, nodeId), idx) =>
                stepToNodeId.getOrElseUpdate(
                  s"${step}|?${idx + 1}",
                  MSet.empty,
                ) += nodeId
            }
      }
      dumpJson(
        name = s"stepToNodeId for ${func.name}",
        data = stepToNodeId,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/stepToNodeId/${func.id}.json",
        silent = true,
      )
    }
  }

  val visited: MSet[Int] = MSet.empty
  private def walk(
    node: Node,
    prevLocOpt: Option[Loc],
    stepToNodeId: MMap[String, MSet[Int]],
    abruptMap: MMap[String, MMap[Pos, Int]],
  ): Unit =
    def safeAdd(key: String, value: Int) =
      stepToNodeId.getOrElseUpdate(key, MSet.empty) += value

    def add(curLocOpt: Option[Loc], prevLocOpt: Option[Loc]): Unit =
      visited += node.id
      (node.loc, prevLocOpt) match
        case (None, _) =>
        case (Some(curLoc), None) =>
          safeAdd(curLoc.stepString, node.id)
        case (Some(curLoc), Some(prevLoc)) =>
          if (curLoc.steps != prevLoc.steps)
            safeAdd(curLoc.stepString, node.id)

    if (!visited.contains(node.id)) {
      node match
        case Block(_, insts, next) =>
          var plo = prevLocOpt
          insts.foreach((inst) => {
            add(inst.loc, plo)
            plo = inst.loc
          })
          next
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
        case Call(_, _, next) =>
          add(node.loc, prevLocOpt)
          next
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
        case Branch(_, _, cond, isAbruptNode, thenNode, elseNode) =>
          if (isAbruptNode && node.loc.isDefined && thenNode.isDefined)
            abruptMap.getOrElseUpdate(
              node.loc.get.stepString,
              MMap(),
            ) += (node.loc.get.start -> thenNode.get.id)
          else if (!isCompTCheck(cond)) {
            thenNode.foreach(tn =>
              (node.loc, tn.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  safeAdd(l1.stepString + "|if", node.id)
                  safeAdd(l2.stepString + "|then", tn.id)
                case _ => (),
            )

            elseNode.foreach(en =>
              (node.loc, en.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  safeAdd(l1.stepString + "|if", node.id)
                  safeAdd(l2.stepString + "|else", en.id)
                case _ => (),
            )
          }

          add(node.loc, prevLocOpt)
          thenNode
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
          elseNode
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
    }

    def isCompTCheck(cond: Expr): Boolean = cond match
      case ETypeCheck(_, IRType(CompT, None)) => true
      case _                                  => false
}
