package esmeta.web

import esmeta.cfg.CFG
import esmeta.es.Ast
import esmeta.web.util.JsonProtocol
import io.circe.*, io.circe.syntax.*

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  _debugger = Some(Debugger(cfg.init.from(sourceText)))
def initDebugger(debugger: Debugger): Unit =
  _debugger = Some(debugger)
private var _debugger: Option[Debugger] = None

extension (stepResult: Debugger.StepResult)
  def withAdditional(
    reprint: Boolean = false,
  )(using Encoder[Ast], CFG): Json =
    val webJsonProtocol = JsonProtocol(summon[CFG])
    import webJsonProtocol.given
    Json.fromFields(
      List(
        "result" -> stepResult.ordinal.asJson,
        "callstack" -> debugger.callStackInfo.asJson,
        "stepCnt" -> debugger.getStepCnt.asJson,
        "instCnt" -> debugger.getInstCnt.asJson,
        "heap" -> debugger.heapInfo.asJson,
      ) ++ {
        if (reprint) then
          List(
            "reprint" -> debugger.st.sourceText.asJson,
            "ast" -> debugger.st.cachedAst
              .map(_.asJson(using webJsonProtocol.astEncoder))
              .getOrElse(Json.Null),
          )
        else Nil
      },
    )
