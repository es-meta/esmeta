package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.constructor.{Builder}

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
    Builder(
      cfg,
      nodeToProgId,
      stepToNode,
      progIdToProg,
      noLocFunc,
      targetNodes,
      funcToEcId,
      ecIdToFunc,
    )

//    val keyValuePairs = for {
//      (outerKey, outerMap) <- nodeToProgId
//      (innerKey, innerMap) <- outerMap
//      (leafKey, value) <- innerMap
//    } yield (outerKey, innerKey, leafKey, value)
//
//    val total = keyValuePairs.size
//    var iter = 1;

//    println("=========== Calculating Iteration Count =========")
//    keyValuePairs.foreach {
//      case (node, feature, callPath, (filename, intValue))
//          if Files.exists(Paths.get(s"$MINIMAL_DIR/$filename")) =>
//        println(s"$iter/$total : $filename running")
//        iter += 1
//        new Constructor(
//          cfg.init.fromFile(s"$MINIMAL_DIR/$filename"),
//          node,
//          feature,
//          callPath,
//          nodeToProgId,
//        ).result
//    }

    println("=========== Dump =========")
    dump()

  /* { nodeId : { feature : { callPath : progId } } } */
  val nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]] =
    MMap()

  /* { algId : { algName, { step : nodeId } } */
  val stepToNode: MMap[String, MMap[String, String]] =
    MMap()

  /* { progId : prog } */
  val progIdToProg: MMap[String, String] = MMap()

  val targetNodes: MSet[String] = MSet()
  val noLocFunc: MSet[String] = MSet()

  val funcToEcId: MMap[String, MSet[String]] = MMap()
  val ecIdToFunc: MMap[String, String] = MMap()

  val RECENT_DIR = s"$FUZZ_LOG_DIR/fuzz-250103_11_39"
  private val MINIMAL_DIR = s"$RECENT_DIR/minimal"
  private val noSpace = false

  def dump(): Unit =
    dumpJson(
      "target-nodes.json",
      targetNodes,
      s"$RECENT_DIR/target-nodes.json",
      noSpace,
    )
    dumpJson(
      "no-loc-functions.json",
      noLocFunc,
      s"$RECENT_DIR/no-loc-functions.json",
      noSpace,
    )
    dumpJson(
      "step-to-node.json",
      stepToNode,
      s"$RECENT_DIR/step-to-node.json",
      noSpace,
    )
    dumpJson(
      "node-to-progId.json",
      nodeToProgId,
      s"$RECENT_DIR/node-to-progId.json",
      noSpace,
    )
    dumpJson(
      "progId-to-prog.json",
      progIdToProg,
      s"$RECENT_DIR/progId-to-prog.json",
      noSpace,
    )
    dumpJson(
      "func-to-ecId.json",
      funcToEcId,
      s"$RECENT_DIR/func-to-ecId.json",
      noSpace,
    )
    dumpJson(
      "ecId-to-func.json",
      ecIdToFunc,
      s"$RECENT_DIR/ecId-to-func.json",
      noSpace,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
  )
  case class Config()
}
