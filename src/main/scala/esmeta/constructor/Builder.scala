package esmeta.constructor

import esmeta.cfg.*
import esmeta.es.util.JsonProtocol
import esmeta.util.SystemUtils.*
import io.circe.*

import java.lang.Integer.*
import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.phase.Construct.RECENT_DIR

object Builder {
  def apply(
    cfg: CFG,
    nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]],
    stepToNode: MMap[String, MMap[String, String]],
    progIdToProg: MMap[String, String],
    noLocFuncs: MSet[String],
    funcToEcId: MMap[String, String],
    ecIdToFunc: MMap[String, String],
  ): Unit =
    new Builder(
      cfg,
      nodeToProgId,
      stepToNode,
      progIdToProg,
      noLocFuncs,
      funcToEcId,
      ecIdToFunc,
    ).build
}

class Builder(
  cfg: CFG,
  nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]],
  stepToNode: MMap[String, MMap[String, String]],
  progIdToProg: MMap[String, String],
  noLocFuncs: MSet[String],
  funcToEcId: MMap[String, String],
  ecIdToFunc: MMap[String, String],
) {
  val jsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.{*, given}

  def build: Unit = {
    fillStepToNode
    fillNodeToProgIdToProg
    fillNoLocFuncs
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private val targetNodeSet: MSet[String] = MSet()

  /* ToDo - only handle nodes that are corresponding to step */
  private def fillNodeToProgIdToProg: Unit =
    val nviList = readJson[List[TmpNodeViewInfo]](
      s"$RECENT_DIR/node-coverage.json",
    )
    nviList.foreach {
      case TmpNodeViewInfo(
            _,
            TmpNodeView(TmpNode(_, inst, func), tmpView),
            script,
          ) =>
        val nodeId = inst.split(":").head

        if (targetNodeSet.contains(nodeId)) {
          val feature = tmpView.map(_._2).getOrElse("no-feature")

          val path = tmpView.map(_._3).getOrElse("no-path")

          nodeToProgId.get(nodeId) match {
            case Some(featureMap) =>
              featureMap.get(feature) match {
                case Some(pathMap) =>
                  pathMap.get(path) match
                    case Some(script) => println(s"Same Path ${path}")
                    case None         => pathMap += (path -> (script, 1))
                case None => featureMap += feature -> MMap(path -> (script, 1))
              }
            case None =>
              nodeToProgId += (nodeId -> MMap(
                feature -> MMap(path -> (script, 1)),
              ))
          }

          val code = readFile(s"$RECENT_DIR/minimal/$script")
          if (code != "") {
            progIdToProg.get(script) match
              case Some(_) =>
              case None    => progIdToProg += (script -> code)
          }
        }
    }

  private def fillNoLocFuncs: Unit =
    for {
      func <- cfg.funcs
      if func.nodes.isEmpty || func.nodes.forall(_.loc.isEmpty)
    } noLocFuncs.add(func.name)

  private def fillStepToNode: Unit =
    for {
      func <- cfg.funcs
      node <- func.nodes
      loc <- node.loc
      alg <- func.irFunc.algo
      ecId = alg.head.emuClauseId
    } {
      funcToEcId += (func.name -> ecId)
      ecIdToFunc += (ecId -> func.name)

      val mapping = loc.stepString -> node.id.toString
      stepToNode.get(ecId) match {
        case Some(steps) =>
          steps.get(loc.stepString) match {
            case Some(prevNodeId)
                if (parseInt(prevNodeId) > parseInt(node.id.toString)) =>
              steps += mapping
              targetNodeSet += node.id.toString
            case Some(_) =>
            case None =>
              steps += mapping
              targetNodeSet += node.id.toString
          }
        case None =>
          stepToNode += ecId -> MMap(mapping)
          targetNodeSet += node.id.toString
      }

      node match {
        case Branch(_, _, _, isAbruptNode, Some(thenNode), _)
            if isAbruptNode && thenNode.loc.isDefined =>
          targetNodeSet += thenNode.id.toString
          stepToNode.get(ecId) match {
            case Some(steps) =>
              steps += (generateUniqueKey(
                thenNode.loc.get.stepString,
                steps,
              ) -> thenNode.id.toString)
            case None =>
              stepToNode += ecId ->
              MMap(
                generateUniqueKey(
                  thenNode.loc.get.stepString,
                ) -> thenNode.id.toString,
              )
          }
        case _ =>
      }
    }

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
