package esmeta.constructor

import esmeta.cfg.*
import esmeta.error.NoBoolean
import esmeta.state.*
import esmeta.web.Debugger

import scala.collection.mutable.Map as MMap

class Constructor(
  st: State,
  targetNodeId: String,
  targetFeature: String,
  targetCallPath: String,
  nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]],
) extends Debugger(st) {
  private var flag = true;
  private inline def cfg = st.cfg

  override def eval(node: Node): Unit =
    if (
      flag && node.id.toString == targetNodeId && st.context.callPath.toString == targetCallPath
    ) {
      flag = false
      val (script, cnt) =
        nodeToProgId(targetNodeId)(targetFeature)(targetCallPath)
      import scala.util.Try
      val rawPath = targetCallPath
      val nodeIdList = "\\d+".r
        .findAllIn(rawPath)
        .toList

      val pathAlgList = nodeIdList.map { nodeIdStr =>
        Try(nodeIdStr.toInt).toOption
          .flatMap { nodeIdNum =>
            cfg.nodeMap.get(nodeIdNum).flatMap { node =>
              val step = node.loc match
                case Some(l) => l.stepString
                case None    => nodeIdStr
              cfg.funcOf.get(node).map(_.name + s"/${step}")
            }
          }
          .getOrElse(nodeIdStr)
      }

      val path =
        if (rawPath == "no-path") "no-path"
        else pathAlgList.mkString("<")

      nodeToProgId(targetNodeId)(targetFeature) -= targetCallPath;
      nodeToProgId(targetNodeId)(
        targetFeature,
      ) += path -> (script, getIter - 1)
    }

    super.eval(node)

}
