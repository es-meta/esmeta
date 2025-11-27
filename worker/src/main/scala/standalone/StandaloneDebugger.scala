package worker

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.dump.debugger.util.JsonProtocol.given
import esmeta.es.Ast
import esmeta.ir.*
import esmeta.spec.*
import esmeta.state.DynamicAddr
import esmeta.ty.*
import esmeta.util.ManualInfo
import esmeta.web.{*, given}
import esmeta.web.util.JsonProtocol as WebJsonProtocol
import esmeta.util.BaseUtils.raise

import io.circe.*, io.circe.syntax.*
import io.circe.parser.*

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js;
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.JSConverters.*

import worker.util.*

@JSExportTopLevel("StandaloneDebuggerClass")
class StandaloneDebugger(cfg: CFG, predefined: String) {
  given CFG = cfg
  val webJsonProtocol = WebJsonProtocol(cfg)
  given WebJsonProtocol = webJsonProtocol

  var _debugger: Option[Debugger] = None

  def debugger: Debugger = _debugger.get

  private def initDebugger(cfg: CFG, sourceText: String): Unit =
    val next = Some(Debugger(cfg.init.from(sourceText)))
    _debugger = next
  end initDebugger

  initDebugger(cfg, "");

  /** conversion for HTTP response */
  given Conversion[Debugger.StepResult, String] = _.ordinal.toString

  /* meta routes */
  @JSExport def meta_debugString(): String = ""
  @JSExport def meta_version(): String = s"${esmeta.VERSION}-js".asJson.noSpaces
  @JSExport def meta_iter(): String = debugger.getStepCnt.asJson.noSpaces

  /* spec routes */
  @JSExport def spec_func(): String = predefined
  @JSExport def spec_version(): String = cfg.spec.version.asJson.noSpaces
  @JSExport def spec_irToSpecNameMap(): Nothing = raise("handle this in worker")

  /* breakpoint routes */
  @JSExport def breakpoint_add(raw: String): String = {
    decode[(Boolean, String, List[Int], Boolean)](raw) match
      case Left(err)   => ??? // TODO handle error
      case Right(data) => debugger.addBreak(data).asJson.noSpaces
  }
  @JSExport def breakpoint_remove(raw: String): String = {
    decode[Int](raw) match
      case Right(idx)              => debugger.rmBreak(idx)
      case Left(_) if raw == "all" => debugger.rmBreakAll
      case Left(err)               => ??? // TODO handle error
    "null"
  }
  @JSExport def breakpoint_toggle(raw: String): String = {
    decode[Int](raw) match
      case Right(idx)              => debugger.toggleBreak(idx)
      case Left(_) if raw == "all" => debugger.toggleBreakAll
      case _                       => ??? // TODO handle error
    "null"
  }

  /* exec routes */

  // TODO change raw string to better type
  // decode can be done by browser when using Web Worker
  @JSExport def exec_run(raw: String) = {
    decode[(String, List[(Boolean, String, List[Int], Boolean)])](
      raw,
    ) match
      case Left(err) => ??? // TODO handle error
      case Right((sourceText, bpDatas)) =>
        initDebugger(cfg, sourceText)
        for { data <- bpDatas } debugger.addBreak(data)
        Debugger.StepResult.ReachedFront
          .withAdditional(debugger, reprint = true)
          .noSpaces
  }
  @JSExport def exec_resumeFromIter(raw: String): String = {
    decode[(String, List[(Boolean, String, List[Int], Boolean)], Int)](
      raw,
    ) match
      case Left(err) => ??? // TODO handle error
      case Right((sourceText, bpData, iterCount)) =>
        initDebugger(cfg, sourceText)
        for { data <- bpData } debugger.addBreak(data)
        debugger
          .stepExactly(iterCount, true)
          .withAdditional(debugger, reprint = true)
          .noSpaces
  }
  @JSExport def exec_backToProvenance(raw: String): String = {
    decode[String](
      raw,
    ) match
      case Left(err)   => ??? // TODO handle error
      case Right(addr) =>
        // ToDo - support named address
        // ToDo - handle NumberFormatException
        debugger
          .stepBackToProvenance(
            DynamicAddr(addr.filter(_.isDigit).toLong),
          )
          .withAdditional(debugger)
          .noSpaces
  }
  @JSExport def exec_step(noBreak: Boolean): String = debugger
    .specStep(
      Debugger.StepOptions(noBreak),
    )
    .withAdditional(debugger)
    .noSpaces
  @JSExport def exec_stepOver(noBreak: Boolean): String =
    debugger
      .specStepOver(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_stepOut(noBreak: Boolean): String =
    debugger
      .specStepOut(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_stepBack(noBreak: Boolean): String =
    debugger
      .specStepBack(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_stepBackOver(noBreak: Boolean): String =
    debugger
      .specStepBackOver(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_stepBackOut(noBreak: Boolean): String =
    debugger
      .specStepBackOut(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_continue(): String =
    debugger.continue.withAdditional(debugger).noSpaces
  @JSExport def exec_rewind(): String =
    debugger.rewind.withAdditional(debugger).noSpaces
  @JSExport def exec_irStep(noBreak: Boolean): String =
    debugger
      .irStep(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_irStepOver(noBreak: Boolean): String =
    debugger
      .irStepOver(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_irStepOut(noBreak: Boolean): String =
    debugger
      .irStepOut(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_esAstStep(noBreak: Boolean): String =
    debugger
      .esAstStep(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_esStatementStep(noBreak: Boolean): String = debugger
    .esStatementStep(Debugger.StepOptions(noBreak))
    .withAdditional(debugger)
    .noSpaces
  @JSExport def exec_esStepOver(noBreak: Boolean): String =
    debugger
      .esStepOver(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_esStepOut(noBreak: Boolean): String =
    debugger
      .esStepOut(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_stepCntPlus(noBreak: Boolean): String =
    debugger
      .stepCntPlus(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_StepCntMinus(noBreak: Boolean): String =
    debugger
      .stepCntMinus(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_instCntPlus(noBreak: Boolean): String =
    debugger
      .instCntPlus(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
  @JSExport def exec_instCntMinus(noBreak: Boolean): String =
    debugger
      .instCntMinus(Debugger.StepOptions(noBreak))
      .withAdditional(debugger)
      .noSpaces
}

@JSExportTopLevel("StandaloneDebugger")
object StandaloneDebugger {

  @JSExport
  def buildFrom(
    input: Input,
    rateCallback: js.UndefOr[js.Function1[Double, Unit]] = js.undefined,
  ): js.Promise[StandaloneDebugger] =
      withMeasure("build") {
        try {
        val funcsFuture =
          (decodeListWithMeasure[Func]("Func")(input.funcs, rateCallback))
        val versionFuture =
          (decodeWithMeasure[Spec.Version]("Version")(input.version))
        val grammarFuture =
          (decodeWithMeasure[Grammar]("Grammar")(input.grammar))
        val tablesFuture =
          (decodeWithMeasure[Map[String, Table]]("Tables")(input.tables))
        val tyModelFuture = Future(ManualInfo.tyModel)
        val intrinsicsFuture = Future(ManualInfo.intrinsics)

        for {
          funcs <- funcsFuture
          version <- versionFuture
          grammar <- grammarFuture
          tables <- tablesFuture
          tyModel <- tyModelFuture
          intrinsics <- intrinsicsFuture
          _ = println("All data decoded successfully")
          spec = Spec(
            Some(version),
            grammar,
            /* not needed for now, algo = */ Nil,
            tables,
            tyModel,
            intrinsics,
          )
        } yield {

          println("Spec created successfully")

          val cfg = benchmark { CFGBuilder.apply(Program.apply(funcs, spec)) } {
            time =>
              println(s"CFG created successfully, time taken: ${time}ms")
          }

          val service = benchmark { StandaloneDebugger(cfg, input.funcsCfg) } {
            time =>
              println(
                s"Mocking Interface created successfully, Time taken: ${time} ms",
              )
          }

          service
        }
        } catch {
          case e: Throwable =>
            println(s"Error during building StandaloneDebugger: ${e}")
            println(e.getMessage())
            e.printStackTrace()
            throw e
        }
    }.toJSPromise
}
