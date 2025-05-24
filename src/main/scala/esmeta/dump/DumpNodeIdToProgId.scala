package esmeta.dump

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

object DumpNodeIdToProgId {
  def trimCode(path: String): String = readFile(path)
    .trim()
    .replace("\"use strict\";\n", "")

  def getStepCnt(
    st: State,
    targetNodeId: Int,
    targetCallPath: String,
    printMsg: () => Unit,
  ): Int =
    var stepCnt = 1
    try new StepCounter(st, targetNodeId, targetCallPath).result
    catch
      case e =>
        printMsg()
        stepCnt = e.getMessage().toInt
    return stepCnt

  def apply(cfg: CFG): Unit =
    val jsonProtocol = VisualizerJsonProtocol(cfg)
    import jsonProtocol.{*, given}
    val nvList =
      readJson[List[NodeViewInfoJson]](s"$RECENT_DIR/node-coverage.json")

    val nodeIdToProgId: MMap[Int, MMap[String, MMap[String, (Int, Int)]]] =
      MMap.empty
    val progIdSet: MSet[Int] = MSet.empty

    val total = nvList.length
    nvList.zipWithIndex.foreach {
      case (NodeViewInfoJson(_, NodeViewJson(node, view), scriptStr), idx) =>
        val script = scriptStr.toInt
        progIdSet += script

        val featIdToProgId = nodeIdToProgId.getOrElse(node.id, MMap.empty)
        val currentCode = trimCode(s"$RECENT_DIR/minimal/$script.js")

        view match
          case Some(ViewJson(enclosing, feature, path)) =>
            val featId = feature.id.toString
            val pathStr = path.getOrElse("")

            val stepCnt = getStepCnt(
              cfg.init.from(currentCode),
              node.id,
              pathStr,
              () => println(s"${idx + 1}/$total"),
            )

            val cpToProgId =
              featIdToProgId.getOrElseUpdate(featId, MMap.empty)
            cpToProgId.getOrElseUpdate(pathStr, (script, stepCnt))

            val min = featIdToProgId.getOrElseUpdate("minimal", MMap.empty)
            min.get("minimal") match
              case Some(scriptId, _) =>
                if (
                  trimCode(
                    s"$RECENT_DIR/minimal/$scriptId.js",
                  ).length > currentCode.length
                ) min += "minimal" -> (script, stepCnt)
              case None => min += "minimal" -> (script, stepCnt)
          case None =>
            val stepCnt = getStepCnt(
              cfg.init.from(currentCode),
              node.id,
              "",
              () => println(s"${idx + 1}/$total"),
            )

            featIdToProgId
              .getOrElseUpdate("minimal", MMap.empty)
              .getOrElseUpdate("minimal", (script, stepCnt))

            featIdToProgId
              .getOrElseUpdate("", MMap.empty)
              .getOrElseUpdate("", (script, stepCnt))
        nodeIdToProgId += (node.id -> featIdToProgId)
    }

    progIdSet.foreach { progId =>
      val codeWithOutUseStrict = readFile(s"$RECENT_DIR/minimal/$progId.js")
        .trim()
        .replace("\"use strict\";\n", "")
      dumpJson(
        name = s"progIdToScript for $progId",
        data = codeWithOutUseStrict,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/progIdToScript/${progId}.json",
        silent = true,
      )
    }

    nodeIdToProgId.foreach {
      case (nodeId, featIdToProgId) =>
        dumpJson(
          name = s"nodeIdToProgId for $nodeId",
          data = featIdToProgId,
          filename = s"$DUMP_VISUALIZER_LOG_DIR/nodeIdToProgId/${nodeId}.json",
          silent = true,
        )
    }

  val RECENT_DIR = s"$FUZZ_LOG_DIR/recent"
}
