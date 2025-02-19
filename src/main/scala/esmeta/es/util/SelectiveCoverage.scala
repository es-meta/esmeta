package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.fuzzer.*
import esmeta.es.util.injector.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EReturnIfAbrupt, EBool}
import esmeta.state.*
import esmeta.ty.AstSingleTy
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import math.Ordering.Implicits.seqOrdering
import esmeta.es.JSTrans

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import esmeta.es.util.Coverage.*

class SelectiveCoverage(
  timeLimit: Option[Int] = None,
  cp: Boolean = false,
  selectiveConfig: SelectiveConfig,
  fixed: Boolean = false,
) extends Coverage(
    timeLimit,
    selectiveConfig.maxSensitivity,
    cp,
  ) {

  var targetFeatSet = new TargetFeatureSet(selectiveConfig)

  def transpilableRate: Double =
    _minimalInfo.values.count(_.transpilable.getOrElse(false))
    / _minimalInfo.values.count(_.transpilable.isDefined).toDouble

  override def check(
    script: Script,
    interp: Coverage.Interp,
  ): (State, Boolean, Boolean) =
    val Script(code, name) = script
    val codeWithUseStrict = USE_STRICT + code + LINE_SEP
    val isTranspilerHitFuture = Future {
      if fixed then None
      else
        selectiveConfig.targetTrans match
          case "swc" =>
            JSTrans.checkTranspileDiffSrvOpt(codeWithUseStrict, Some("swc"))
          case "babel" =>
            JSTrans.checkTranspileDiffSrvOpt(codeWithUseStrict, Some("babel"))
          case "terser" =>
            JSTrans.checkTranspileDiffSrvOpt(codeWithUseStrict, Some("terser"))
          case "swcES2015" =>
            JSTrans.checkTranspileDiffSrvOpt(
              codeWithUseStrict,
              Some("swcES2015"),
            )
          case _ => None
    }

    val initSt =
      cfg.init.from(code) // TODO: Check if recreating init state is OK
    val finalSt = interp.result

    // covered new elements
    var covered = false
    // updated elements
    var updated = false

    val rawStacks = (interp.touchedNodeViews.keys
      .flatMap(_.view) ++
      interp.touchedCondViews.keys.flatMap(_.view))
      .map(v => (v._2 :: v._1).map(_.func.name))
      .toSet

    // update node coverage
    for ((rawNodeView, _) <- interp.touchedNodeViews)
      val NodeView(node, rawView) = rawNodeView
      val view = rawView.flatMap {
        case (enclosing, feature, path) =>
          val rawStack = feature :: enclosing
          val featureStack =
            rawStack.take(targetFeatSet((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val nodeView = NodeView(node, view)

      getScript(nodeView) match
        case None =>
          update(nodeView, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(nodeView, script); updated = true
        case _ =>

    // update branch coverage
    for ((rawCondView, nearest) <- interp.touchedCondViews)
      val CondView(cond, rawView) = rawCondView
      val view = rawView.flatMap {
        case (enclosing, feature, path) =>
          val rawStack = feature :: enclosing
          val featureStack =
            rawStack.take(targetFeatSet((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val condView = CondView(cond, view)

      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script); updated = true
        case _ =>

    val isTranspilerHitOpt = Await.result(isTranspilerHitFuture, 10.seconds)

    isTranspilerHitOpt match
      case Some(true) =>
        targetFeatSet.touchWithHit(rawStacks)
      case Some(false) =>
        targetFeatSet.touchWithMiss(rawStacks)
      case _ => ()

    // update script info
    if (updated)
      _minimalInfo += script.name -> Coverage.ScriptInfo(
        ConformTest.createTest(initSt, finalSt),
        interp.touchedNodeViews.map(_._1),
        interp.touchedCondViews.map(_._1),
        transpilable = isTranspilerHitOpt,
      )
    // assert: _minimalScripts ~= _minimalInfo.keys

    (finalSt, updated, covered)

  override def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withScriptInfo: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withMsg: Boolean = true,
    isEnd: Boolean = false,
  ): Unit =
    super.dumpTo(
      baseDir,
      withScripts,
      withScriptInfo,
      withTargetCondViews,
      withUnreachableFuncs,
      withMsg,
      isEnd = true,
    )
    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1000000 / 1e3
    def log(msg: Any) =
      if (withMsg) println(s"[$elapsedSec s] $msg")
    import TargetFeatureSet.given
    dumpJson(
      name = "target feature set",
      data = targetFeatSet,
      filename = s"$baseDir/tfset.json",
      space = true,
    )
    log("dumped target feature set")

}

object SelectiveCoverage {

  def filterMinimals(baseDir: String, targetDir: String): Unit =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    def rj[T](json: String)(implicit decoder: Decoder[T]) =
      readJson[T](s"$baseDir/$json")

    val con: CoverageConstructor = rj(s"constructor.json")

    val targetFeatSet = TargetFeatureSet.fromDir(baseDir)
    val proAlpha = 0.001
    val demAlpha = 0.005
    val newTFS = targetFeatSet.copy(
      config = targetFeatSet.config.copy(
        promotionThreshold = proAlpha,
        demotionThreshold = demAlpha,
      ),
      targetFeatureMap = targetFeatSet.targetFeatureMap.map {
        case (k, v) =>
          k -> v.copy(status =
            if (v.hits + v.misses > 10) then
              if (v.hits.toDouble / (v.hits + v.misses) > 1 - proAlpha)
                TargetFeatureStatus.Noticed
              else TargetFeatureStatus.Ignored
            else TargetFeatureStatus.Ignored,
          )
      },
    )

    val cov = new SelectiveCoverage(
      timeLimit = con.timeLimit,
      cp = con.cp,
      selectiveConfig = newTFS.config,
      fixed = true,
    )
    cov.targetFeatSet = newTFS

    import esmeta.util.ProgressBar

    val progress = ProgressBar(
      msg = "Filter minimals",
      iterable = listFiles(s"$baseDir/minimal"),
    )

    for (minimal <- progress) {
      val name = minimal.getName
      val code = readFile(minimal.getPath).drop(USE_STRICT.length).trim
      val script = Script(code, name)

      cov.runAndCheck(script)
    }
    cov.dumpToWithDetail(targetDir)

    println(s"Number of filtered minimals: ${cov.minimalScripts.size}")

}
