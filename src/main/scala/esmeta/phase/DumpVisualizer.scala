package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.ir.*
import esmeta.ir.{Type => IRType, Func => irFunc}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.state.*
import esmeta.spec.*
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.{BasicParsers}
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.collection.mutable.ListBuffer
import esmeta.phase.DumpVisualizer.name
import esmeta.es.util.Coverage.NodeViewInfo
import esmeta.cfg.Func
import esmeta.error.ESMetaError
import _root_.esmeta.es.util.Node
import esmeta.interpreter.Interpreter
import cats.instances.order

/** `dump-visualizer` phase */
case object DumpVisualizer extends Phase[CFG, Unit] {
  val name = "dump-visualizer"
  val help =
    "dumps the resources required by the visualizer. (for internal use)"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {

    val (secIdToFuncId, secIdToFuncName) = DumpSecIdToFuncInfo(cfg)
    DumpStepToNodeId(cfg)
    DumpNodeIdToProgId(cfg)
    DumpNodeIdToTest262(cfg)
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}

object DumpSecIdToFuncInfo {
  def apply(cfg: CFG): (MMap[String, Int], MMap[String, String]) =
    val secIdToFuncId: MMap[String, Int] = MMap.empty
    val secIdToFuncName: MMap[String, String] = MMap.empty

    for {
      func <- cfg.funcs
      irFunc = func.irFunc
      algo <- func.irFunc.algo
      sectionId = algo.elem.parent.id
    } {
      val secId =
        if (func.isSDO && func.sdoInfo.isDefined)
          s"$sectionId|${extractSDO(func.sdoInfo.get, cfg)}"
        else sectionId

      secIdToFuncId += (secId -> func.id)
      secIdToFuncName += (secId -> convertFuncName(func, cfg))
    }
    dumpJson(
      name = "secIdToFuncId",
      data = secIdToFuncId,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFuncId.json",
      silent = true,
    )
    dumpJson(
      name = "secIdToFuncName",
      data = secIdToFuncName,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFuncName.json",
      silent = true,
    )
    (secIdToFuncId, secIdToFuncName)

  def convertFuncName(func: Func, cfg: CFG): String =
    if (func.isMethod)
      val parsed = parseFuncName(func)
      if (func.kind == FuncKind.InternalMeth) s"[[$parsed]]" else parsed
    else if (func.isBuiltin) func.name.substring("INTRINSICS.".length)
    else if (func.isSDO) {
      func.sdoInfo.get match
        case SdoInfo.Default(func, method) => method
        case SdoInfo.Base(_, name, i, j, method) =>
          val buffer: ListBuffer[String] = ListBuffer.empty
          buffer += s"[$method] $name :"
          cfg.grammar.prods.find(_.name == name).foreach { prod =>
            val rhs = prod.rhsVec(i)
            rhs.getSymbols(j).flatMap(x => x).foreach { symbol =>
              (symbol.getT, symbol.getNt) match
                case (Some(t), None)  => buffer += t.term
                case (None, Some(nt)) => buffer += nt.name
                case _                =>
            }
          }
          buffer.mkString(" ")
    } else func.name

  def parseFuncName(func: Func): String =
    JsonParser.parseAll(JsonParser.methodName, func.name) match {
      case JsonParser.Success(result, _) => result._2
      case _ => throw ESMetaError(s"invalid method name ${func.name}")
    }

  // sectionId | astName | production
  def extractSDO(sdoInfo: SdoInfo, cfg: CFG): String = sdoInfo match
    case SdoInfo.Default(func, method) => "ToDo"
    case SdoInfo.Base(_, name, i, j, method) =>
      val buffer: ListBuffer[String] = ListBuffer.empty
      buffer += name
      cfg.grammar.prods.find(_.name == name).foreach { prod =>
        val rhs = prod.rhsVec(i)
        rhs.getSymbols(j).flatMap(x => x).foreach { symbol =>
          (symbol.getT, symbol.getNt) match
            case (Some(t), None)  => buffer += t.term
            case (None, Some(nt)) => buffer += nt.name
            case _                =>
        }
      }
      buffer.mkString("|")
}

object DumpStepToNodeId {
  import esmeta.util.{given_Ordering_Pos}

  def apply(cfg: CFG): Unit =
    for {
      func <- cfg.funcs
      algo <- func.irFunc.algo
      sectionId = algo.elem.parent.id
    } {
      val stepToNodeId: MMap[String, MSet[Int]] = MMap.empty
      val abruptMap: MMap[String, MMap[Pos, Int]] = MMap.empty
      walk(func.entry, None, stepToNodeId, abruptMap)

      abruptMap.foreach {
        case (step, posSet) =>
          posSet.toSeq
            .sorted(Ordering.by[(Pos, Int), Pos](_._1))
            .zipWithIndex
            .foreach {
              case ((pos, nodeId), idx) =>
                stepToNodeId.getOrElseUpdate(
                  s"${step}|?${idx + 1}",
                  MSet.empty,
                ) += nodeId
            }
      }
      dumpJson(
        name = s"stepToNodeId for ${func.name}",
        data = stepToNodeId,
        filename = s"$DUMP_VISUALIZER_LOG_DIR/stepToNodeId/${func.id}.json",
        silent = true,
      )
    }

  val visited: MSet[Int] = MSet.empty
  private def walk(
    node: Node,
    prevLocOpt: Option[Loc],
    stepToNodeId: MMap[String, MSet[Int]],
    abruptMap: MMap[String, MMap[Pos, Int]],
  ): Unit =
    def safeAdd(key: String, value: Int) =
      stepToNodeId.getOrElseUpdate(key, MSet.empty) += value

    def add(curLocOpt: Option[Loc], prevLocOpt: Option[Loc]): Unit =
      visited += node.id
      (node.loc, prevLocOpt) match
        case (None, _) =>
        case (Some(curLoc), None) =>
          safeAdd(curLoc.stepString, node.id)
        case (Some(curLoc), Some(prevLoc)) =>
          if (curLoc.steps != prevLoc.steps)
            safeAdd(curLoc.stepString, node.id)

    if (!visited.contains(node.id)) {
      node match
        case Block(_, insts, next) =>
          var plo = prevLocOpt
          insts.foreach((inst) => {
            add(inst.loc, plo)
            plo = inst.loc
          })
          next
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
        case Call(_, _, next) =>
          add(node.loc, prevLocOpt)
          next
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
        case Branch(_, _, cond, isAbruptNode, thenNode, elseNode) =>
          if (isAbruptNode && node.loc.isDefined && thenNode.isDefined)
            abruptMap.getOrElseUpdate(
              node.loc.get.stepString,
              MMap(),
            ) += (node.loc.get.start -> thenNode.get.id)
          else if (!isCompTCheck(cond)) {
            thenNode.foreach(tn =>
              (node.loc, tn.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  safeAdd(l1.stepString + "|if", node.id)
                  safeAdd(l2.stepString + "|then", tn.id)
                case _ => (),
            )

            elseNode.foreach(en =>
              (node.loc, en.loc) match
                case (Some(l1), Some(l2)) if (l1.stepString == l2.stepString) =>
                  safeAdd(l1.stepString + "|if", node.id)
                  safeAdd(l2.stepString + "|else", en.id)
                case _ => (),
            )
          }

          add(node.loc, prevLocOpt)
          thenNode
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
          elseNode
            .foreach(walk(_, node.loc, stepToNodeId, abruptMap))
    }

    def isCompTCheck(cond: Expr): Boolean = cond match
      case ETypeCheck(_, IRType(CompT, None)) => true
      case _                                  => false
}

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
    val jsonProtocol = NodeCoverageJsonProtocol(cfg)
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

object DumpNodeIdToTest262 {

  def apply(cfg: CFG): Unit =
    val jsonProtocol = NodeCoverageJsonProtocol(cfg)
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
  val TEST262_ID_MAPPING = s"$TEST262TEST_LOG_DIR/test262-test-id-mapping.json"
}

case class NodeViewInfoJson(
  index: Int,
  nodeView: NodeViewJson,
  script: String,
)

case class NodeViewJson(
  node: NodeJson,
  view: Option[ViewJson],
)

case class ViewJson(
  enclosing: List[Feature],
  feature: Feature,
  path: Option[String],
)

case class NodeJson(
  id: Int,
  inst: String,
  func: Func,
)

class NodeCoverageJsonProtocol(cfg: CFG) {
  def funcNameToFeature(fName: String): Feature = {
    val func = cfg.getFunc(fName)
    func.head match
      case Some(head: SyntaxDirectedOperationHead) =>
        SyntacticFeature(func, head)
      case Some(head: BuiltinHead) =>
        BuiltinFeature(func, head)
      case _ =>
        throw ESMetaError(s"invalid feature $fName")
  }

  given nodeViewInfoDecoder: Decoder[NodeViewInfoJson] = Decoder.instance(c =>
    for {
      index <- c.downField("index").as[Int]
      nodeView <- c.downField("nodeView").as[NodeViewJson]
      script <- c.downField("script").as[String].map(_.stripSuffix(".js"))
    } yield NodeViewInfoJson(index, nodeView, script),
  )

  given nodeViewDecoder: Decoder[NodeViewJson] = Decoder.instance(c =>
    for {
      node <- c.downField("node").as[NodeJson]
      view <- c.downField("view").as[Option[ViewJson]]
    } yield NodeViewJson(node, view),
  )

  def parseNodeName(name: String): Int = {
    val result = JsonParser.parseAll(JsonParser.nodeName, name)
    result match
      case JsonParser.Success(value, _) => value
      case _ => throw ESMetaError(s"invalid node name $name")
  }
  given NodeDecoder: Decoder[NodeJson] = Decoder.instance(c =>
    for {
      id <- c.downField("name").as[String].map(parseNodeName(_))
      inst <- c.downField("inst").as[String]
      func <- c.downField("func").as[String].map(cfg.getFunc)
    } yield NodeJson(id, inst, func),
  )

  def parseCallPath(path: Option[String]): Option[String] = {
    def parse(path: String): String = {
      val result = JsonParser.parseAll(JsonParser.callPath, path)
      result match
        case JsonParser.Success(nodeList, _) =>
          nodeList
            .flatMap { nodeId =>
              val node = cfg.nodeMap(nodeId)
              val funcId = cfg.funcOf(node).id
              node.loc.map(loc => s"$funcId|${loc.stepString}")
            }
            .mkString("-")
        case JsonParser.NoSuccess(msg, next) =>
          throw ESMetaError(
            s"invalid call path $path: $msg at line ${next.pos.line}, column ${next.pos.column}",
          )
        case _ => throw ESMetaError(s"invalid call path $path")
    }
    path match
      case None       => None
      case Some(path) => Some(parse(path))
  }

  given ViewDecoder: Decoder[ViewJson] = Decoder.instance(c =>
    for {
      enclosing <- c
        .downField("enclosing")
        .as[List[String]]
        .map(_.map(funcNameToFeature))
      feature <- c.downField("feature").as[String].map(funcNameToFeature)
      path <- c.downField("path").as[Option[String]].map(parseCallPath(_))
    } yield ViewJson(enclosing, feature, path),
  )
}

private object JsonParser extends BasicParsers {
  lazy val nodeName: Parser[Int] = ("\\w+".r ~> "[" ~> "\\d+".r <~ "]") ^^ {
    _.toInt
  }

  lazy val callPath: Parser[List[Int]] = {
    ("CallPath[" ~> repsep("\\d+".r ^^ { _.toInt }, "<-") <~ "]")
  }

  lazy val methodName = ("Record[" ~> "\\w+".r <~ "]") ~ ("." ~> "\\w+".r) ^^ {
    case t ~ n => (t, n)
  }
}

class StepCounter(
  st: State,
  targetNodeId: Int,
  targetCallPath: String,
) extends Interpreter(st) {
  override def eval(node: Node): Unit =
    val currentCP = new NodeCoverageJsonProtocol(st.cfg)
      .parseCallPath(Some(st.context.callPath.toString))
      .getOrElse("")
    if (node.id == targetNodeId && (currentCP == targetCallPath)) {
      throw new Exception(stepCnt.toString)
    }

    super.eval(node)
}
