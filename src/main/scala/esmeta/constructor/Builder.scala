package esmeta.constructor

import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.es.util.JsonProtocol
import esmeta.ir.{EClo, ICall, ISdoCall}
import esmeta.phase.{Construct}
import esmeta.util.SystemUtils.*
import io.circe.*

import java.lang.Integer.*
import scala.collection.mutable.{Map as MMap, Set as MSet}
import esmeta.phase.Construct.{
  RECENT_DIR,
  RECENT_TEST262_DIR,
  StepEcIdToCallNodeId,
}
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.util.Loc

class Builder(
  cfg: CFG,
  SPEC_FUNC: List[Func],
  SPEC_FUNC_IDS: MSet[Int],
  StepToNodeId: MMap[Int, MMap[String, MSet[Int]]],
  NodeIdToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int)]]],
  ProgIdToProg: MMap[Int, String],
  EcIdToFunc: MMap[String, MSet[String]],
  FuncToEcId: MMap[String, String],
  FuncIdToFunc: MMap[Int, String],
  FuncToFuncId: MMap[String, Int],
  CallNodeIdToFuncId: MMap[Int, String],
  StepEcIdToCallNodeId: MMap[String, Int],
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
    fillCallNodeToSpec()
    fillStepToNodeImproved()
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

  val abruptSaver: MMap[String, String] = MMap.empty
  private def addNode(
    node: Node,
    map: MMap[String, MSet[Int]],
    befStepBlock: String = "",
    fid: Int = -1,
  ): Unit = {
    if (steps(node).nonEmpty) {
      node match
        case blockNode: Block =>
          val stepList = steps(blockNode)
          var befStep: String = befStepBlock
          for {
            (step, idx) <- stepList.zipWithIndex
          } {
            val nodeIdSet = map.getOrElse(step, MSet.empty)
            if (befStep != step)
              nodeIdSet.add(node.id)
              TargetNodeId += node.id
            befStep = step
            map += step -> nodeIdSet
          }
        case Branch(_, _, _, true, Some(thenNode), _) =>
          val loc =
            node.loc.getOrElse(throw new ESMetaError(s"no loc for $node"))
          val key = s"$fid${loc.stepString}"
          val cnt: Int = abruptSaver.get(key) match
            case Some(locStr) =>
              if (locStr == (loc.stepString + loc.rangeString)) 1
              else 2
            case None =>
              abruptSaver += key -> (loc.rangeString)
              1

          val step = steps(node).head + s"?$cnt"
          val nodeIdSet = map.getOrElse(step, MSet.empty)
          nodeIdSet.add(thenNode.id)
          TargetNodeId += thenNode.id
          map += step -> nodeIdSet
        case branchNode: Branch =>
          val step = steps(node).head
          val nodeIdSet = map.getOrElse(step, MSet.empty)
          nodeIdSet.add(node.id)
          TargetNodeId += node.id
          map += step -> nodeIdSet
        case callNode: Call =>
          val step = steps(node).head
          val nodeIdSet = map.getOrElse(step, MSet.empty)
          nodeIdSet.add(node.id)
          TargetNodeId += node.id
          map += step -> nodeIdSet
    }
  }

  private def fillStepToNodeImproved(): Unit =
    for {
      func <- SPEC_FUNC
      sortedNodes = func.nodes.toList.sortBy(_.id)
      (node, idx) <- sortedNodes.zipWithIndex
    } {
//      if (func.id == 2482) {
//        println(s"#${idx} : ${node} <- ${steps(node)}")
//      }

      val inlineBranchCheck: MMap[String, Boolean] = MMap()

      val funcIdKey = func.id
      val nodeIdVal = node.id
      val outerMap = StepToNodeId.getOrElse(funcIdKey, MMap.empty)

      // first node added
      if (idx == 0)
        addNode(node, outerMap)

      val tmpList = steps(node)
      val currentSteps = if (tmpList.isEmpty) then List("-1") else tmpList

      node match
        case blockNode: Block =>
          val befStep = currentSteps.last
          blockNode.next.foreach {
            case nextNode: Block =>
              if (steps(nextNode).nonEmpty)
                addNode(nextNode, outerMap, befStep)
            case nextNode: NodeWithInst =>
              if (steps(nextNode).nonEmpty && befStep != steps(nextNode).head)
                addNode(nextNode, outerMap)
          }
        case branchNode @ Branch(_, _, _, false, opThen, opElse) =>
          val befStep = currentSteps.head
          opThen.foreach { thenNode =>
            if (steps(thenNode).nonEmpty) {
              if (befStep != steps(thenNode).head)
                addNode(thenNode, outerMap)
              if (befStep == steps(thenNode).head)
                val step = befStep + "then"
                val idSet = outerMap.getOrElse(step, MSet.empty)
                idSet.add(thenNode.id)
                TargetNodeId += thenNode.id
                outerMap += step -> idSet
            }
          }
          opElse.foreach { elseNode =>
            if (steps(elseNode).nonEmpty) {
              if (befStep != steps(elseNode).head)
                addNode(elseNode, outerMap)
              if (befStep == steps(elseNode).head)
                val step = befStep + "else"
                val idSet = outerMap.getOrElse(step, MSet.empty)
                idSet.add(elseNode.id)
                TargetNodeId += elseNode.id
                outerMap += step -> idSet
            }
          }
        case abruptBranchNode @ Branch(_, _, _, true, Some(thenNode), _) =>
          addNode(abruptBranchNode, outerMap, fid = func.id)
        case callNode: Call =>
          val befStep = currentSteps.head
          callNode.next.foreach {
            case nextNode: Block =>
              if (steps(nextNode).nonEmpty)
                addNode(nextNode, outerMap, befStep)
            case nextNode: NodeWithInst =>
              if (steps(nextNode).nonEmpty && befStep != steps(nextNode).head)
                addNode(nextNode, outerMap)
          }
        case _ =>
      StepToNodeId += funcIdKey -> outerMap
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
              Some(TmpView(_, featureName, rawPath)),
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

        val path = generateCallPathString(rawPath)
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
          val codeWithOutUseStrict = code.replace("\"use strict\";\n", "")
          dumpFile(codeWithOutUseStrict, s"$RECENT_DIR/minimal/$scriptId.js")
          if (code != "") {
            ProgIdToProg.get(scriptId) match
              case Some(_) =>
              case None    => ProgIdToProg += (scriptId -> codeWithOutUseStrict)
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

        val path = generateCallPathString(rawPath)
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

  private def fillCallNodeToSpec(): Unit =
    for {
      func <- SPEC_FUNC
      node <- func.nodes
      callerEcId <- FuncToEcId.get(func.name)
    } {
      val stepList = steps(node)

      node match
        case Call(callId, callInst, _) if (stepList.length == 1) =>
          val stepList = steps(node)
          CallNodeIdToFuncId += callId -> (s"${func.id}/${stepList.head.toString}")

          callInst match
            case ICall(_, fexpr, _) =>
              fexpr match
                case EClo(fname, _) if FuncToEcId.contains(fname) =>
                  val calleeEcId = FuncToEcId.getOrElse(
                    fname,
                    throw new ESMetaError(s"[Callee] No ecId for ${fname}"),
                  )
                  val func = cfg.funcOf.getOrElse(
                    node,
                    throw new ESMetaError("no node@@@"),
                  )
                  StepEcIdToCallNodeId += (callerEcId + "/" + stepList.head.toString + "/" + calleeEcId) -> callId

                case other =>
                  extraInfo(
                    s"${func.name} - ${node.id} - ${steps(node)} ## ${other.getClass.getSimpleName}",
                  )
            case ISdoCall(_, base, _, _) =>
        case _ =>
    }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private def steps(node: Node): List[String] =
    val stepList = node match
      case block: Block =>
        block.insts.map(_.loc).collect { case Some(l) => l.stepString }.toList
      case call: Call =>
        List(call.callInst.loc).collect { case Some(l) => l.stepString }
      case branch: Branch =>
        List(branch.cond.loc).collect { case Some(l) => l.stepString }
    stepList.sorted

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

  private def generateCallPathString(rawPath: String): String =
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

//    var counter = 0
//    nodeIdList.foreach((nodeId) =>
//      val node = cfg.nodeMap.getOrElse(nodeId, throw new ESMetaError("!!!!"))
//      val func = cfg.funcOf.getOrElse(node, throw new ESMetaError("&&&&&"))
//      val tmp = CallNodeIdToFuncId.get(nodeId)
//
//      tmp match
//        case None if counter == 1 => throw new ESMetaError(s"No Corresponding Call Node ${func.name} - ${node}")
//        case None => counter +=1
//        case _ =>
//    )

//    val funcIdList: List[Int] = nodeIdList.flatMap { nodeId =>
//      cfg.nodeMap.get(nodeId).flatMap { node =>
//        cfg.funcOf.get(node).map(_.id)
//      }
//    }

    val path =
      if nodeIdList.isEmpty then "" else nodeIdList.mkString("<")
    path

}
