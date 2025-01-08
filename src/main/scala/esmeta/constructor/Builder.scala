package esmeta.constructor

import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.es.util.JsonProtocol
import esmeta.util.SystemUtils.*
import io.circe.*

import java.lang.Integer.*
import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.phase.Construct.{RECENT_DIR, RECENT_TEST262_DIR, StepToNodeId}
import esmeta.spec.Algorithm
import esmeta.util.Loc

class Builder(
  cfg: CFG,
  StepToNodeId: MMap[Int, MMap[String, Int]],
  NodeIdToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int, String)]]],
  ProgIdToProg: MMap[Int, String],
  EcIdToFunc: MMap[String, MSet[String]],
  FuncToEcId: MMap[String, String],
  FuncIdToFunc: MMap[Int, String],
  NoLocFunc: MSet[Int],
  TargetNodeId: MSet[Int],
  extraInfo: Boolean = false,
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
      _ = FuncIdToFunc += (func.id -> func.name)
    } func.irFunc.algo match {
      case Some(Algorithm(_, _, _, ecId)) =>
        val ecFuncIds = EcIdToFunc.getOrElseUpdate(ecId, MSet.empty)
        ecFuncIds += func.name

        if (ecFuncIds.size == 2)
          extraInfo(s"More than one func : ${func.irFunc.kind} for a ${ecId}")

        FuncToEcId += (func.name -> ecId)
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
      val funcIdKey = func.id
      val nodeIdVal = node.id
      val stepToNodeId = step -> nodeIdVal

      StepToNodeId.get(funcIdKey) match {
        case Some(stepToNodeIds) =>
          stepToNodeIds.get(step) match {
            case Some(prevNodeId) if prevNodeId > nodeIdVal =>
              stepToNodeIds += stepToNodeId
              TargetNodeId += nodeIdVal
            case None =>
              stepToNodeIds += stepToNodeId
              TargetNodeId += nodeIdVal
            case _ =>
          }
        case None =>
          StepToNodeId += funcIdKey -> MMap(stepToNodeId)
          TargetNodeId += nodeIdVal
      }

      node match {
        case Branch(_, _, _, true, Some(thenNode), _)
            if thenNode.loc.isDefined =>
          val thenNodeId = thenNode.id
          TargetNodeId += thenNodeId
          StepToNodeId.get(funcIdKey) match {
            case Some(stepToNodeIds) =>
              stepToNodeIds += (generateUniqueKey(
                step,
                stepToNodeIds,
              ) -> thenNodeId)
            case None =>
              StepToNodeId += funcIdKey ->
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
    /* ToDo change TmpNodeView */
    val nviList = readJson[List[TmpNodeViewInfo]](
      s"$RECENT_DIR/node-coverage.json",
    )
    val test262List = readJson[List[TmpNodeViewInfo]](
      s"$RECENT_TEST262_DIR/node-coverage.json",
    )
    nviList.foreach {
      case TmpNodeViewInfo(
            _,
            TmpNodeView(
              TmpNode(_, inst, func),
              Some(TmpView(_, featureName, path)),
            ),
            script,
          ) =>
        val nodeId = parseInt(inst.split(":").head) // get node id
        val featureId = cfg.fnameMap
          .getOrElse(
            featureName,
            throw new ESMetaError(
              s"[Builder] no corresponding feature for $featureName",
            ),
          )
          .id
        val scriptId = parseInt(script.stripSuffix(".js"))

        if (TargetNodeId.contains(nodeId)) {
          NodeIdToProgId.get(nodeId) match {
            case Some(featureMap) =>
              featureMap.get(featureId) match {
                case Some(pathMap) =>
                  pathMap.get(path) match
                    case Some(script) => extraInfo(s"Same Path $path")
                    case None         => pathMap += (path -> (scriptId, 1, ""))
                case None =>
                  featureMap += featureId -> MMap(
                    path -> (scriptId, 1, ""),
                  )
              }
            case None =>
              NodeIdToProgId += (nodeId -> MMap(
                featureId -> MMap(path -> (scriptId, 1, "")),
              ))
          }

          val code = readFile(s"$RECENT_DIR/minimal/$scriptId.js")
          if (code != "") {
            ProgIdToProg.get(scriptId) match
              case Some(_) =>
              case None    => ProgIdToProg += (scriptId -> code)
          }
        }
      case TmpNodeViewInfo(
            _,
            TmpNodeView(TmpNode(_, inst, func), _),
            _,
          ) =>
        extraInfo(s"[Uncollected Node View] $func : $inst")
    }

    test262List.foreach {
      case TmpNodeViewInfo(
            _,
            TmpNodeView(
              TmpNode(_, inst, _),
              Some(TmpView(_, featureName, path)),
            ),
            encoded,
          ) =>
        val nodeId = parseInt(inst.split(":").head)
        val featureId = cfg.fnameMap
          .getOrElse(
            featureName,
            throw new ESMetaError(
              s"[Builder] no corresponding feature for $featureName",
            ),
          )
          .id
        if (TargetNodeId.contains(nodeId)) {
          NodeIdToProgId.get(nodeId) match {
            case Some(featureMap) =>
              featureMap.get(featureId) match {
                case Some(pathMap) =>
                  pathMap.get(path) match
                    case Some(script) =>
                      val save = pathMap.get(path)
                      pathMap += (path -> (save.get._1, save.get._2 + 1, encoded))
                    case None => pathMap += (path -> (-1, -1, encoded))
                case None =>
                  featureMap += featureId -> MMap(
                    path -> (-1, -1, encoded),
                  )
              }
            case None =>
              NodeIdToProgId += (nodeId -> MMap(
                featureId -> MMap(path -> (-1, -1, encoded)),
              ))
          }
        }
      case _ =>
    }

  private def fillNoLocFunc(): Unit =
    for {
      func <- cfg.funcs
      if func.nodes.isEmpty || func.nodes.forall(_.loc.isEmpty)
    } NoLocFunc.add(func.id)

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

  private def extraInfo(str: String): Unit = if (extraInfo) println(str)

  private def generateUniqueKey(
    baseKey: String,
    steps: MMap[String, Int] = null,
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
