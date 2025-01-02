package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.constructor.{Builder, Constructor, Analyzer}
import esmeta.interpreter.Interpreter
import esmeta.state.State

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
//    println("=========== Building =========")
//    Builder(
//      cfg,
//      nodeToProgId,
//      stepToNode,
//      progIdToProg,
//      noLocFuncs,
//      funcToEcId,
//      ecIdToFunc,
//    )
//
//    val keyValuePairs = for {
//      (outerKey, outerMap) <- nodeToProgId
//      (innerKey, innerMap) <- outerMap
//      (leafKey, value) <- innerMap
//    } yield (outerKey, innerKey, leafKey, value)

    println("=========== Handling Abruption ===========")
    val st = new Analyzer(
      cfg.init.fromFile(s"$BASE_DIR/test.js"),
      2027, // 이거 2028로 고쳐야됨
    ).apply()

//    println("=========== Calculating Iteration Count =========")
//    keyValuePairs.foreach {
//      case (node, feature, callpath, (filename, intValue))
//          if (Files.exists(Paths.get(s"$MINIMAL_DIR/$filename"))) =>
//        println(s"$filename running")
//        new Constructor(
//          cfg.init.fromFile(s"$MINIMAL_DIR/$filename"),
//          node,
//          feature,
//          callpath,
//          nodeToProgId,
//        ).result
//    }
//
//    println("=========== Dump =========")
//    dump

  val nodeToProgId: MMap[String, MMap[String, MMap[String, (String, Int)]]] =
    MMap()
  /* { nodeId : { feature : { callPath : progId } } } */
  val stepToNode: MMap[String, MMap[String, String]] =
    MMap()
  /* { algId : { algName, { step : nodeId } } */
  val progIdToProg: MMap[String, String] = MMap()
  /* { progId : prog } */
  val noLocFuncs: MSet[String] = MSet()

  val funcToEcId: MMap[String, String] = MMap();
  val ecIdToFunc: MMap[String, String] = MMap();

  def dump: Unit =
    dumpJson(
      "no-loc-functions.json",
      noLocFuncs,
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

  val RECENT_DIR = s"$FUZZ_LOG_DIR/fuzz-241223_06_58_1"
  val MINIMAL_DIR = s"${RECENT_DIR}/minimal"
  private val noSpace = false

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
  )
  case class Config()
}
