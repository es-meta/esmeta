package esmeta.constructor

import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.es.util.JsonProtocol
import esmeta.util.SystemUtils.*
import io.circe.*

import java.lang.Integer.*
import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.phase.Construct.{RECENT_DIR, RECENT_TEST262_DIR}
import esmeta.spec.SyntaxDirectedOperationHead.*
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.util.Loc

class Builder(
  cfg: CFG,
  SPEC_FUNC: List[Func],
  SPEC_FUNC_IDS: MSet[Int],
  StepToNodeId: MMap[Int, MMap[String, Int]],
  NodeIdToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int)]]],
  ProgIdToProg: MMap[Int, String],
  EcIdToFunc: MMap[String, MSet[String]],
  FuncToEcId: MMap[String, String],
  FuncIdToFunc: MMap[Int, String],
  FuncToFuncId: MMap[String, Int],
  NoLocFunc: MSet[Int],
  TargetNodeId: MSet[Int],
  NodeIdToTest262: MMap[Int, MMap[Int, MMap[String, String]]],
  extraInfoFlag: Boolean = false,
) {
  /* ToDO */
  val jsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.{*, given}

  def build(): Unit =
    fillFuncToEcId()
    fillStepToNode()
    fillNodeToProgIdToProg()
    fillNodeToTest262()
    fillNoLocFunc()

  private def fillFuncToEcId(): Unit =
    for {
      func <- SPEC_FUNC
      _ = FuncIdToFunc += (func.id -> func.name)
      _ = FuncToFuncId += func.name -> func.id
    } func.irFunc.algo match {
      case Some(Algorithm(_, _, _, ecId)) =>
        val ecFuncIds = EcIdToFunc.getOrElseUpdate(ecId, MSet.empty)
        ecFuncIds += func.name

        if (ecFuncIds.size == 2)
          extraInfo(s"More than one func : ${func.irFunc.kind} for a $ecId")

        FuncToEcId += (func.name -> ecId)
      case None =>
        extraInfo(
          s"[FillFuncToEcId] Algorithm not found for function: ${func.name}",
        )
    }

  private def fillStepToNode(): Unit =
    for {
      func <- SPEC_FUNC
      node <- func.nodes
      step <- steps(node)
    } {
      val funcIdKey = func.id
      val nodeIdVal = node.id
      val stepToNodeId = step -> nodeIdVal

      // collect steps
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

      // collect return if abrupt
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

    // collect branch
    for {
      func <- SPEC_FUNC
      irFunc = func.irFunc
      node <- func.nodes
      step <- steps(node)
    } {
      val funcIdKey = func.id
      node match
        case Branch(_, _, _, false, optionThen, optionElse) =>
          for {
            thenNode <- optionThen
            if(steps(node) == steps(thenNode))
          }
            val map = StepToNodeId.getOrElse(funcIdKey, MMap())
            map.get(s"${step}then") match
              case None =>
                TargetNodeId += thenNode.id
                map += (s"${step}then" -> thenNode.id)
              case Some(nodeId) =>
                if(nodeId > thenNode.id)
                  TargetNodeId += thenNode.id
                  map += (s"${step}then" -> thenNode.id)
            StepToNodeId += funcIdKey -> map
          for {
            elseNode <- optionElse
            if (steps(node) == steps(elseNode))
          }
            TargetNodeId += elseNode.id
            val map = StepToNodeId.getOrElse(funcIdKey, MMap())
            map.get(s"${step}else") match
              case None =>
                TargetNodeId += elseNode.id
                map += (s"${step}else" -> elseNode.id)
              case Some(nodeId) =>
                if(nodeId > elseNode.id)
                  TargetNodeId += elseNode.id
                  map += (s"${step}else" -> elseNode.id)
            StepToNodeId += funcIdKey -> map
        case _ =>
    }

  private def fillNodeToProgIdToProg(): Unit =
    /* ToDo change TmpNodeView */
    val nviList = readJson[List[TmpNodeViewInfo]](
      s"$RECENT_DIR/node-coverage.json",
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
                    case None         => pathMap += (path -> (scriptId, 1))
                case None =>
                  featureMap += featureId -> MMap(
                    path -> (scriptId, 1),
                  )
              }
            case None =>
              NodeIdToProgId += (nodeId -> MMap(
                featureId -> MMap(path -> (scriptId, 1)),
              ))
          }

          val code = readFile(s"$RECENT_DIR/minimal/$scriptId.js")
          if (code != "") {
            ProgIdToProg.get(scriptId) match
              case Some(_) =>
              case None    => ProgIdToProg += (scriptId -> code)
          }
        } else extraInfo(s"[Builder] $featureId is not a spec function id")
      case TmpNodeViewInfo(
            _,
            TmpNodeView(TmpNode(_, inst, func), _),
            _,
          ) =>
        extraInfo(s"[Builder] view is null - $func : $inst")
    }

  private def fillNodeToTest262(): Unit =
    /* ToDo change TmpNodeView */
    val test262List = readJson[List[TmpNodeViewInfo]](
      s"$RECENT_TEST262_DIR/node-coverage.json",
    )
    test262List.foreach {
      case TmpNodeViewInfo(
            _,
            TmpNodeView(
              TmpNode(_, inst, func),
              Some(TmpView(_, featureName, rawPath)),
            ),
            encoded,
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

        val path = generateCallpathString(rawPath)
        if (TargetNodeId.contains(nodeId)) {
          NodeIdToTest262.get(nodeId) match {
            case Some(featureMap) =>
              featureMap.get(featureId) match {
                case Some(pathMap) =>
                  pathMap.get(path) match
                    case Some(script) => extraInfo(s"Same Path $path")
                    case None         => pathMap += (path -> encoded)
                case None =>
                  featureMap += featureId -> MMap(
                    path -> encoded,
                  )
              }
            case None =>
              NodeIdToTest262 += (nodeId -> MMap(
                featureId -> MMap(path -> encoded),
              ))
          }
        } else extraInfo(s"[Builder] $featureId is not a spec function id")
      case TmpNodeViewInfo(
            _,
            TmpNodeView(TmpNode(_, inst, func), _),
            _,
          ) =>
        extraInfo(s"[Builder] view is null - $func : $inst")
    }

  private def fillNoLocFunc(): Unit =
    for {
      func <- SPEC_FUNC
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

  private def extraInfo(str: String): Unit = if (extraInfoFlag) println(str)

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

  private def generateCallpathString(rawPath : String) : String =
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
    path

}
