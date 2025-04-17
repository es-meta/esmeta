package worker

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.dump.util.JsonProtocol.given
import esmeta.es.Ast
import esmeta.ir.*
import esmeta.spec.*
import esmeta.state.DynamicAddr
import esmeta.ty.*
import esmeta.web.Debugger
import esmeta.util.BaseUtils.error

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import io.circe.parser.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js;
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.scalajs.js.JSConverters.*

import worker.util.*

@JSExportTopLevel("StandaloneDebuggerClass")
class StandaloneDebugger(cfg: CFG) {

  var _debugger: Option[Debugger] = None
  var _lastParsed: Option[(String, Ast)] = None

  def debugger: Debugger = _debugger.get

  private def initDebugger(cfg: CFG, sourceText: String): Unit =
    val ast = _lastParsed match
      case Some(oldText, ast) if oldText == sourceText =>
        ast
      case _ =>
        val ast = cfg.scriptParser.from(sourceText)
        _lastParsed = Some((sourceText, ast))
        ast
    _debugger = Some(Debugger(cfg.init.from(sourceText, ast, None)))
  end initDebugger

  initDebugger(cfg, "");

  /** conversion for HTTP response */
  given Conversion[Debugger#StepResult, String] = _.ordinal.toString

  /* meta routes */
  @JSExport def meta_debugString(): String = ""
  @JSExport def meta_version(): String = s"${esmeta.VERSION}-js".asJson.noSpaces
  @JSExport def meta_iter(): String = debugger.getStepCnt.asJson.noSpaces

  /* state routes */
  @JSExport def state_heap(): String = debugger.heapInfo.asJson.noSpaces
  @JSExport def state_context(cid: Int): String =
    debugger.ctxtInfo(cid).asJson.noSpaces
  @JSExport def state_callStack(): String =
    debugger.callStackInfo.asJson.noSpaces

  /* spec routes */
  @JSExport def spec_func(): String =
    cfg.fnameMap.map { case (name, f) => (f.id, name) }.toList.asJson.noSpaces
  @JSExport def spec_version(): String = cfg.spec.version.asJson.noSpaces
  @JSExport def spec_irToSpecNameMap(): Nothing = error("handle this in worker")

  /* breakpoint routes */
  @JSExport def breakpoint_add(raw: String): String = {
    decode[(Boolean, Int, List[Int], Boolean)](raw) match
      case Left(err) => ??? // TODO handle error
      case Right(data) =>
        debugger.addBreak(data)
        "null"
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
    decode[(String, List[(Boolean, Int, List[Int], Boolean)])](
      raw,
    ) match
      case Left(err) => ??? // TODO handle error
      case Right((sourceText, bpDatas)) =>
        initDebugger(cfg, sourceText)
        for { data <- bpDatas } debugger.addBreak(data)
        "null"
  }
  @JSExport def exec_resumeFromIter(raw: String): String = {
    decode[(String, List[(Boolean, Int, List[Int], Boolean)], Int)](
      raw,
    ) match
      case Left(err) => ??? // TODO handle error
      case Right((sourceText, bpData, iterCount)) =>
        initDebugger(cfg, sourceText)
        for { data <- bpData } debugger.addBreak
        debugger.stepExactly(iterCount, true)
    "null"
  }
  @JSExport def exec_backToProvenance(raw: String): String = {
    decode[String](
      raw,
    ) match
      case Left(err)   => ??? // TODO handle error
      case Right(addr) =>
        // ToDo - support named address
        // ToDo - handle NumberFormatException
        debugger.stepBackToProvenance(
          DynamicAddr(addr.filter(_.isDigit).toLong),
        )
  }
  @JSExport def exec_step(noBreak: Boolean): String = debugger.specStep(noBreak)
  @JSExport def exec_stepOver(noBreak: Boolean): String =
    debugger.specStepOver(noBreak)
  @JSExport def exec_stepOut(noBreak: Boolean): String =
    debugger.specStepOut(noBreak)
  @JSExport def exec_stepBack(noBreak: Boolean): String =
    debugger.specStepBack(noBreak)
  @JSExport def exec_stepBackOver(noBreak: Boolean): String =
    debugger.specStepBackOver(noBreak)
  @JSExport def exec_stepBackOut(noBreak: Boolean): String =
    debugger.specStepBackOut(noBreak)
  @JSExport def exec_continue(): String = debugger.continue
  @JSExport def exec_rewind(): String = debugger.rewind
  @JSExport def exec_irStep(noBreak: Boolean): String = debugger.irStep(noBreak)
  @JSExport def exec_irStepOver(noBreak: Boolean): String =
    debugger.irStepOver(noBreak)
  @JSExport def exec_irStepOut(noBreak: Boolean): String =
    debugger.irStepOut(noBreak)
  @JSExport def exec_esAstStep(): String = debugger.esAstStep
  @JSExport def exec_esStatementStep(): String = debugger.esStatementStep
  @JSExport def exec_esStepOver(): String = debugger.esStepOver
  @JSExport def exec_esStepOut(): String = debugger.esStepOut
  @JSExport def exec_iterPlus(noBreak: Boolean): String =
    debugger.iterPlus(noBreak)
  @JSExport def exec_iterMinus(noBreak: Boolean): String =
    debugger.iterMinus(noBreak)
}

@JSExportTopLevel("StandaloneDebugger")
object StandaloneDebugger {

  @JSExport
  def buildFrom(
    input: Input,
  ): js.Promise[StandaloneDebugger] =
    measureFutureTime {
      withMeasure("build") {

        val funcsFuture = (decodeWithMeasure[List[Func]]("Funcs")(input.funcs))
        val versionFuture =
          (decodeWithMeasure[Spec.Version]("Version")(input.version))
        val grammarFuture =
          (decodeWithMeasure[Grammar]("Grammar")(input.grammar))
        val tablesFuture =
          (decodeWithMeasure[Map[String, Table]]("Tables")(input.tables))
        val tyModelFuture =
          (decodeWithMeasure[TyModel]("TyModel")(input.tyModel))

        for {
          funcs <- funcsFuture
          version <- versionFuture
          grammar <- grammarFuture
          tables <- tablesFuture
          tyModel <- tyModelFuture
          _ = println("All data decoded successfully")
          spec = Spec(
            Some(version),
            grammar,
            /* not needed for now, algo = */ Nil,
            tables,
            tyModel,
          )
        } yield {

          println("Spec created successfully")

          val cfg = benchmark { CFGBuilder.apply(Program.apply(funcs, spec)) } {
            time =>
              println(s"CFG created successfully, time taken: ${time}ms")
          }

          val service = benchmark { StandaloneDebugger(cfg) } { time =>
            println(
              s"Mocking Interface created successfully, Time taken: ${time} ms",
            )
          }

          service
        }

      }
    }.toJSPromise
}
