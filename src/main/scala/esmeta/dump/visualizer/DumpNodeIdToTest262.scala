package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.dump.util.VisualizerJsonProtocol
import esmeta.error.ESMetaError
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{ListBuffer, Map as MMap, Set as MSet}

object DumpNodeIdToTest262 {

  def apply(cfg: CFG): Unit =
    val jsonProtocol = VisualizerJsonProtocol(cfg)
    import jsonProtocol.{*, given}

    val nodeIdToTest262: MMap[Int, MMap[String, MMap[String, String]]] =
      MMap.empty

    val nvList =
      readJson[List[NodeViewInfoJson]](s"$RECENT_DIR/node-coverage.json")
    val total = nvList.length
    nvList.zipWithIndex.foreach {
      case (NodeViewInfoJson(_, NodeViewJson(node, view), encoded), idx) =>
        val featIdToProgId = nodeIdToTest262.getOrElse(node.id, MMap.empty)
        view match
          case Some(ViewJson(enclosing, feature, path)) =>
            featIdToProgId
              .getOrElseUpdate(feature.id.toString, MMap.empty)
              .getOrElseUpdate(path.getOrElse(""), encoded)
          case None =>
            featIdToProgId
              .getOrElseUpdate("", MMap.empty)
              .getOrElseUpdate("", encoded)
        nodeIdToTest262 += (node.id -> featIdToProgId)
    }

    nodeIdToTest262.foreach {
      case (nodeId, featIdToProgId) =>
        dumpJson(
          name = s"nodeIdToTest262 for $nodeId",
          data = featIdToProgId,
          filename = s"$DUMP_VISUALIZER_LOG_DIR/nodeIdToTest262/${nodeId}.json",
          silent = true,
        )
    }

  val RECENT_DIR = s"$TEST262TEST_LOG_DIR/recent"
  val TEST262_ID_MAPPING = s"$TEST262TEST_LOG_DIR/test262IdToTest262.json"
}
