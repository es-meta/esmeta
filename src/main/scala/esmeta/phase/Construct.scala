package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
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
    println("=========== Building =========")
    new Builder(
      cfg,
      StepToNodeId,
      NodeIdToProgId,
      ProgIdToProg,
      EcIdToFunc,
      FuncToEcId,
      FuncIdToFunc,
      NoLocFunc,
      TargetNodeId,
    ).build()

    /* { nodeId : { featureFuncId : { callPathFuncIdString : (progId,IterCnt,test262bitvector } } } */
    val keyValuePairs = for {
      (nodeId, fncMap) <- NodeIdToProgId
      (featureId, cpMap) <- fncMap
      (cp, value) <- cpMap
    } yield (nodeId, featureId, cp, value)

    val total = keyValuePairs.size
    var iter = 1

    println("=========== Calculating Iteration Count =========")
    keyValuePairs.foreach {
      case (node, feature, callPath, (filename, _, encode))
          if (Files.exists(
            Paths.get(s"$MINIMAL_DIR/$filename.js"),
          ) && filename == -1) =>
        println(s"$iter/$total : $filename running")
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

  /* { funcId : { step : nodeId } } */
  val StepToNodeId: MMap[Int, MMap[String, Int]] = MMap()
  /* { nodeId : { featureFuncId : { callPathFuncIdString : (progId,IterCnt,test262bitvector } } } */
  val NodeIdToProgId: MMap[Int, MMap[Int, MMap[String, (Int, Int, String)]]] =
    MMap()
  /* { progId : prog } */
  val ProgIdToProg: MMap[Int, String] = MMap()

  val FuncToEcId: MMap[String, String] = MMap()
  val EcIdToFunc: MMap[String, MSet[String]] = MMap()
  val FuncIdToFunc: MMap[Int, String] = MMap()

  val TargetNodeId: MSet[Int] = MSet()
  val NoLocFunc: MSet[Int] = MSet()

  val RECENT_DIR = s"$FUZZ_LOG_DIR/fuzz-250103_11_39"
  val RECENT_TEST262_DIR = s"$TEST262TEST_LOG_DIR/eval-250108_02_03"
  private val MINIMAL_DIR = s"$RECENT_DIR/minimal"
  private val noSpace = false

  private def dump(): Unit =
    dumpJson(
      "target-nodeId.json",
      TargetNodeId,
      s"$RECENT_DIR/target-nodeId.json",
      noSpace,
    )
    dumpJson(
      "no-loc-function.json",
      NoLocFunc,
      s"$RECENT_DIR/no-loc-function.json",
      noSpace,
    )
    dumpJson(
      "step-to-nodeId.json",
      StepToNodeId,
      s"$RECENT_DIR/step-to-nodeId.json",
      noSpace,
    )
    dumpJson(
      "nodeId-to-progId.json",
      NodeIdToProgId,
      s"$RECENT_DIR/nodeId-to-progId.json",
      noSpace,
    )
    dumpJson(
      "progId-to-prog.json",
      ProgIdToProg,
      s"$RECENT_DIR/progId-to-prog.json",
      noSpace,
    )
    dumpJson(
      "func-to-ecId.json",
      FuncToEcId,
      s"$RECENT_DIR/func-to-ecId.json",
      noSpace,
    )
    dumpJson(
      "ecId-to-func.json",
      EcIdToFunc,
      s"$RECENT_DIR/ecId-to-func.json",
      noSpace,
    )
    dumpJson(
      "funcId-to-func.json",
      FuncIdToFunc,
      s"$RECENT_DIR/funcId-to-func.json",
      noSpace,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
  )
  case class Config()
}
