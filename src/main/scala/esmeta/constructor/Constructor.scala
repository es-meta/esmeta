package esmeta.constructor

import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.state.*
import esmeta.web.Debugger

import scala.collection.mutable.Map as MMap

class Constructor(
  st: State,
  targetNodeId: Int,
  targetFeature: Int,
  targetCallPath: String,
  nodeToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int, String)]]],
) extends Debugger(st) {
  private var flag = true
  private inline def cfg = st.cfg

  override def eval(node: Node): Unit =
    if (
      flag && node.id == targetNodeId && st.context.callPath.toString == targetCallPath
    ) {
      flag = false
      val (script, cnt, encode) =
        nodeToProgId
          .getOrElse(
            targetNodeId,
            throw new ESMetaError(
              s"[Constructor] no corresponding nodeId $targetNodeId for nodeToProgId",
            ),
          )
          .getOrElse(
            targetFeature,
            throw new ESMetaError(
              s"[Constructor] no corresponding feature $targetFeature",
            ),
          )
          .getOrElse(
            targetCallPath,
            throw new ESMetaError(
              s"[Constructor] no corresponding callpath $targetCallPath",
            ),
          )

      val rawPath = targetCallPath
      val nodeIdList = "\\d+".r
        .findAllIn(rawPath)
        .toList
        .map { nodeIdStr =>
          nodeIdStr.toIntOption.getOrElse(
            throw new IllegalArgumentException(
              s"Invalid integer value: $nodeIdStr",
            ),
          )
        }

      val funcIdList: List[Int] = nodeIdList.flatMap { nodeId =>
        cfg.nodeMap.get(nodeId).flatMap { node =>
          cfg.funcOf.get(node).map(_.id)
        }
      }

      val path =
        if funcIdList.isEmpty then "ncp" else funcIdList.mkString("<")

      nodeToProgId(targetNodeId)(targetFeature) -= targetCallPath
      nodeToProgId(targetNodeId)(
        targetFeature,
      ) += path -> (script, getIter - 1, encode)
    }
    super.eval(node)

}
