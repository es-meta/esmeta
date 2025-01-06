package esmeta.constructor

import esmeta.cfg.*
import esmeta.es.util.JsonProtocol
import esmeta.util.SystemUtils.*
import io.circe.*

import java.lang.Integer.*
import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.phase.Construct.RECENT_DIR
import esmeta.spec.Algorithm
import esmeta.util.Loc

object Builder {
  def apply(
    cfg: CFG,
    nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]],
    stepToNode: MMap[String, MMap[String, String]],
    progIdToProg: MMap[String, String],
    noLocFunc: MSet[String],
    targetNodes: MSet[String],
    funcToEcId: MMap[String, MSet[String]],
    ecIdToFunc: MMap[String, String],
  ): Unit =
    new Builder(
      cfg,
      nodeToProgId,
      stepToNode,
      progIdToProg,
      noLocFunc,
      targetNodes,
      funcToEcId,
      ecIdToFunc,
    ).build()
}

class Builder(
  cfg: CFG,
  nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]],
  stepToNode: MMap[String, MMap[String, String]],
  progIdToProg: MMap[String, String],
  noLocFunc: MSet[String],
  targetNodes: MSet[String],
  funcToEcId: MMap[String, MSet[String]],
  ecIdToFunc: MMap[String, String],
  extraInfo: Boolean = true
) {
  val jsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.{*, given}

  def build(): Unit =
    fillFuncToEcId()
    fillStepToNode()
    fillNodeToProgIdToProg()
    fillNoLocFunc()

  private def fillFuncToEcId(): Unit =
    for {
      func <- cfg.funcs
    } func.irFunc.algo match {
      case Some(Algorithm(_, _, _, ecId)) =>
        val funcEcIds = funcToEcId.getOrElseUpdate(func.name, MSet.empty)
        funcEcIds += ecId
        
        if (funcEcIds.size == 2)
          extraInfo(s"Several EcId for a ${func.name}")

        ecIdToFunc += (ecId -> func.name)
      case None =>
        extraInfo(
          s"[FillFuncToEcId] Algorithm not found for function: ${func.name}",
        )
    }

  private def fillStepToNode(): Unit =
    for {
      func <- cfg.funcs
      node <- func.nodes
      step <- steps(node)
    } {
      val funcNameKey = func.name
      val nodeIdVal = node.id.toString
      val stepToNodeId = step -> nodeIdVal

      stepToNode.get(funcNameKey) match {
        case Some(stepToNodeIds) =>
          stepToNodeIds.get(step) match {
            case Some(prevNodeId)
                if parseInt(prevNodeId) > parseInt(nodeIdVal) =>
              stepToNodeIds += stepToNodeId
              targetNodes += nodeIdVal
            case None =>
              stepToNodeIds += stepToNodeId
              targetNodes += nodeIdVal
            case _ =>
          }
        case None =>
          stepToNode += funcNameKey -> MMap(stepToNodeId)
          targetNodes += nodeIdVal
      }

      node match {
        case Branch(_, _, _, true, Some(thenNode), _)
            if thenNode.loc.isDefined =>
          val thenNodeId = thenNode.id.toString
          targetNodes += thenNodeId
          stepToNode.get(funcNameKey) match {
            case Some(stepToNodeIds) =>
              stepToNodeIds += (generateUniqueKey(
                step,
                stepToNodeIds,
              ) -> thenNodeId)
            case None =>
              stepToNode += funcNameKey ->
              MMap(
                generateUniqueKey(
                  step,
                ) -> thenNodeId,
              )
          }
        case _ =>
      }
    }

  private def fillNodeToProgIdToProg(): Unit =
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

        if (targetNodes.contains(nodeId)) {
          val feature = tmpView.map(_._2).getOrElse("no-feature")

          val path = tmpView.map(_._3).getOrElse("no-path")

          nodeToProgId.get(nodeId) match {
            case Some(featureMap) =>
              featureMap.get(feature) match {
                case Some(pathMap) =>
                  pathMap.get(path) match
                    case Some(script) => println(s"Same Path $path")
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

  private def fillNoLocFunc(): Unit =
    for {
      func <- cfg.funcs
      if func.nodes.isEmpty || func.nodes.forall(_.loc.isEmpty)
    } noLocFunc.add(func.name)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private def steps(node: Node): List[String] = node match
    case block: Block =>
      block.insts.map(_.loc).collect { case Some(l) => l.stepString }.toList
    case call: Call =>
      List(call.callInst.loc).collect { case Some(l) => l.stepString }
    case branch: Branch =>
      List(branch.cond.loc).collect { case Some(l) => l.stepString }

  private def safeParseInt(str: String): Option[Int] = {
    try {
      Some(Integer.parseInt(str))
    } catch {
      case _: NumberFormatException => None
    }
  }

  private def extraInfo(str: String) : Unit = if(extraInfo) println(str)

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
