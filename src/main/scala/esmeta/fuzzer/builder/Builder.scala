package esmeta.fuzzer.builder

import esmeta.cfg.*
import esmeta.cfg.CFG
import esmeta.es.util.JsonProtocol
import esmeta.util.SystemUtils.*
import io.circe.*

import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.FUZZ_LOG_DIR
import esmeta.es.util.Coverage.NodeViewInfo
import java.lang.Integer.*

object Builder {
  def apply(
    cfg: CFG,
  ): Unit =
    new Builder(cfg).build()
}

class Builder(cfg: CFG) {
  val nodeToProgId: MMap[String, MMap[String, String]] =
    MMap() /* { nodeId : { feature : progId } } */
  val stepToNode: MMap[String, StepToNodeBlock] =
    MMap() /* { algId : { algName, { step : nodeId } } */
  val progIdToProg: MMap[String, String] = MMap() /* { progId : prog } */
  val noLocFuncs: MSet[String] = MSet()

  val jsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.{*, given}

  def build(): Unit = {
    collectNode
    dump
  }

  def collectNode: Unit = {
    def readNodeCoverage(): Unit = {
      val nviList = readJson[List[TmpNodeViewInfo]](
        s"$RECENT_DIR/node-coverage.json",
      )
      nviList.foreach {
        case TmpNodeViewInfo(
              _,
              TmpNodeView(TmpNode(_, inst, _), tmpView),
              script,
            ) =>
          val nodeId = inst.split(":").head
          val feature = tmpView.map(_._2).getOrElse("none")
          nodeToProgId.getOrElseUpdate(
            nodeId,
            MMap.empty,
          ) += (feature -> script)

          val code = readFile(s"$RECENT_DIR/minimal/$script")
          progIdToProg.get(script) match
            case Some(_) =>
            case None    => progIdToProg += (script -> code)
      }
    }
    readNodeCoverage()

    for {
      func <- cfg.funcs
      if func.nodes.isEmpty || func.nodes.forall(_.loc.isEmpty)
    } noLocFuncs.add(func.name)

    for {
      func <- cfg.funcs
      node <- func.nodes
      loc <- node.loc
      alg <- func.irFunc.algo
      ecId = alg.head.emuClauseId
    } {
      val mapping = loc.stepString -> node.id.toString
      stepToNode.get(ecId) match {
        case Some(StepToNodeBlock(_, steps)) =>
          steps.get(loc.stepString) match {
            case Some(prevNodeId) =>
              if (parseInt(prevNodeId) > parseInt(node.id.toString))
                steps += mapping
            case None => steps += mapping
          }
        case None =>
          stepToNode += ecId -> StepToNodeBlock(func.name, MMap(mapping))
      }

      node match {
        case Branch(_, _, _, isAbruptNode, Some(thenNode), _)
            if isAbruptNode && thenNode.loc.isDefined =>
          stepToNode.get(ecId) match {
            case Some(StepToNodeBlock(_, steps)) =>
              steps += (generateUniqueKey(
                thenNode.loc.get.stepString,
                steps,
              ) -> thenNode.id.toString)
            case None =>
              stepToNode += ecId -> StepToNodeBlock(
                func.name,
                MMap(
                  generateUniqueKey(
                    thenNode.loc.get.stepString,
                  ) -> thenNode.id.toString,
                ),
              )
          }
        case _ =>
      }
    }
  }

  def dump: Unit =
    dumpJson(
      "no-loc-functions.json",
      noLocFuncs,
      s"$RECENT_DIR/no-loc-functions.json",
      true,
    )
    dumpJson(
      "step-to-node.json",
      stepToNode,
      s"$RECENT_DIR/step-to-node.json",
      true,
    )
    dumpJson(
      "node-to-progId.json",
      nodeToProgId,
      s"$RECENT_DIR/node-to-progId.json",
      true,
    )
    dumpJson(
      "progId-to-prog.json",
      progIdToProg,
      s"$RECENT_DIR/progId-to-prog.json",
      true,
    )

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  private val RECENT_DIR = s"$FUZZ_LOG_DIR/fuzz-241218_11_14"

  case class StepToNodeBlock(
    name: String,
    steps: MMap[String, String],
  )
  given stepToNodeBlockEncoder: Encoder[StepToNodeBlock] =
    Encoder.instance(stnb =>
      val baseJson = Json.obj(
        "name" -> Json.fromString(stnb.name),
      )
      val stepsJson = stnb.steps.map {
        case (key, value) =>
          key -> Json.fromString(value)
      }
      Json.obj(baseJson.asObject.get.toList ++ stepsJson.toList: _*),
    )

  def safeParseInt(str: String): Option[Int] = {
    try {
      Some(Integer.parseInt(str))
    } catch {
      case _: NumberFormatException => None
    }
  }

  private def generateUniqueKey(
    baseKey: String,
    steps: MMap[String, String] = null,
  ): String = {
    var counter = 1
    var newKey = s"$baseKey?$counter"
    while (steps != null && steps.contains(newKey)) {
      counter += 1
      newKey = s"$baseKey?$counter"
    }
    newKey
  }
}
