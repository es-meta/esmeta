package esmeta.phase

import esmeta.*
import esmeta.cfg.{CFG, Func}
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.constructor.{Builder, Constructor}

import scala.collection.mutable.{Map as MMap, Set as MSet}
import java.nio.file.{Files, Paths}

case object Construct extends Phase[CFG, Unit] {
  val name = "construct"
  val help = "construct files for ECMAVisualizer"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    var SPEC_FUNC_IDS: MSet[Int] = MSet()
    val SPEC_FUNC = for {
      func <- cfg.funcs
      irFunc = func.irFunc
      alg <- irFunc.algo
      _ = SPEC_FUNC_IDS += func.id
    } yield func

    println("=========== Building =========")
    new Builder(
      cfg,
      SPEC_FUNC,
      SPEC_FUNC_IDS,
      StepToNodeId,
      NodeIdToProgId,
      ProgIdToProg,
      EcIdToFunc,
      FuncToEcId,
      FuncIdToFunc,
      FuncToFuncId,
      CallNodeIdToFuncId,
      StepEcIdToCallNodeId,
      NoLocFunc,
      TargetNodeId,
      NodeIdToTest262,
    ).build()

    /* { nodeId : { featureFuncId : { callPathFuncIdString : (progId,IterCnt,test262bitvector } } } */
    val keyValuePairs = for {
      (nodeId, fncMap) <- NodeIdToProgId
      (featureId, cpMap) <- fncMap
      (cp, value) <- cpMap
    } yield (nodeId, featureId, cp, value)

    val total = keyValuePairs.size
    var iter = 1

    println(s"Target Nodes : ${total}")

    println("=========== Calculating Iteration Count =========")
    keyValuePairs.foreach {
      case (node, feature, callPath, (filename, _))
          if (Files.exists(
            Paths.get(s"$MINIMAL_DIR/$filename.js"),
          )) =>
        println(s"$iter/$total : $MINIMAL_DIR/$filename running")
        iter += 1
        new Constructor(
          cfg.init.fromFile(s"$MINIMAL_DIR/$filename.js"),
          node,
          feature,
          callPath,
          NodeIdToProgId,
        ).result
    }

    println("=========== Dump =========")
    dump()

  val StepToNodeId: MMap[Int, MMap[String, MSet[Int]]] =
    MMap() /* { funcId : { step : nodeIdSet } } */
  val NodeIdToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int)]]] =
    MMap() /* { nodeId : { featureFuncId : { callPathFuncIdString : [ progId, IterCnt ] } } */
  val NodeIdToTest262: MMap[Int, MMap[Int, MMap[String, String]]] =
    MMap() /* { nodeId : { featureFuncId : { callPathFuncIdString : progId } } } */
  val ProgIdToProg: MMap[Int, String] = MMap() /* { progId : prog } */

  val StepEcIdToCallNodeId: MMap[String, Int] = MMap()
  val CallNodeIdToFuncId: MMap[Int, String] = MMap()

  val FuncToEcId: MMap[String, String] = MMap()
  val EcIdToFunc: MMap[String, MSet[String]] = MMap()
  val FuncIdToFunc: MMap[Int, String] = MMap()
  val FuncToFuncId: MMap[String, Int] = MMap()

  val TargetNodeId: MSet[Int] = MSet()
  val NoLocFunc: MSet[Int] = MSet()

  val RECENT_DIR = s"$FUZZ_LOG_DIR/fuzz-250112_09_00"
  val DUMP_DIR = s"$RECENT_DIR/json-dump"
  val RECENT_TEST262_DIR = s"$TEST262TEST_LOG_DIR/eval-250108_02_03"
  private val MINIMAL_DIR = s"$RECENT_DIR/minimal"
  private val noSpace = false

  private def dump(): Unit =
//    dumpJson(
//      "target-nodeId.json",
//      TargetNodeId,
//      s"$DUMP_DIR/target-nodeId.json",
//      noSpace,
//    )
//    dumpJson(
//      "no-loc-function.json",
//      NoLocFunc,
//      s"$DUMP_DIR/no-loc-function.json",
//      noSpace,
//    )
    dumpJson(
      "step-to-nodeId.json",
      StepToNodeId,
      s"$DUMP_DIR/step-to-nodeId.json",
      noSpace,
    )
    dumpJson(
      "nodeId-to-progId.json",
      NodeIdToProgId,
      s"$DUMP_DIR/nodeId-to-progId.json",
      noSpace,
    )
    dumpJson(
      "progId-to-prog.json",
      ProgIdToProg,
      s"$DUMP_DIR/progId-to-prog.json",
      noSpace,
    )
    dumpJson(
      "func-to-ecId.json",
      FuncToEcId,
      s"$DUMP_DIR/func-to-ecId.json",
      noSpace,
    )
    dumpJson(
      "ecId-to-func.json",
      EcIdToFunc,
      s"$DUMP_DIR/ecId-to-func.json",
      noSpace,
    )
    dumpJson(
      "funcId-to-func.json",
      FuncIdToFunc,
      s"$DUMP_DIR/funcId-to-func.json",
      noSpace,
    )
    dumpJson(
      "func-to-funcId.json",
      FuncToFuncId,
      s"$DUMP_DIR/func-to-funcId.json",
      noSpace,
    )
    dumpJson(
      "nodeId-to-test262.json",
      NodeIdToTest262,
      s"$DUMP_DIR/nodeId-to-test262.json",
      noSpace,
    )
    dumpJson(
      "callId-to-funcId.json",
      CallNodeIdToFuncId,
      s"$DUMP_DIR/callId-to-funcId.json",
      noSpace,
    )
    dumpJson(
      "ecId-to-callId.json",
      StepEcIdToCallNodeId,
      s"$DUMP_DIR/ecId-to-callId.json",
      noSpace,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
  )
  case class Config()
}
