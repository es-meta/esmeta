package esmeta.web

import esmeta.cfg.CFG

import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import io.circe.generic.semiauto.*
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
class MockingInterface(debugger: Debugger, cfg: CFG) {

  /** conversion for HTTP response */
  given Conversion[Debugger#StepResult, String] = _.ordinal.toString

  object state {
    def heap: String = {
      debugger.heapInfo.asJson.noSpaces
    }
    def context(cid: Int): String = {
      debugger.ctxtInfo(cid).asJson.noSpaces
    }
    def callStack: String = {
      debugger.callStackInfo.asJson.noSpaces
    }
  }

  object spec {
    def func: String = {
      cfg.fnameMap
        .map { case (name, f) => (f.id, name) }
        .toList
        .asJson
        .noSpaces
    }
  }
  object exec {
    // fix
    def run(raw: String): String = {
      decode[(String, List[(Boolean, Int, List[Int], Boolean)])](
          raw,
        ) match
          case Left(err) => ??? // TODO handle error
          case Right((sourceText, bpDatas)) =>
            initDebugger(cfg, sourceText)
            for { data <- bpDatas } debugger.addBreak(data)
            "null"
    }
    def step: String = {
      debugger.esStep
    }
    def stepOver: String = {
      debugger.esStepOver
    }
    def stepOut: String = {
      debugger.esStepOut
    }
    def continue: String = {
      debugger.continue
    }

    def esStep: String = {
      debugger.esStep
    }

    def esStepOver: String = {
      debugger.esStepOver
    }

    def esStepOut: String = {
      debugger.esStepOut
    }

  }

  object breakpoint {
    def add(raw: String): String = {
      decode[(Boolean, Int, List[Int], Boolean)](raw) match
        case Left(err) => ??? // TODO handle error
        case Right(data) =>
          debugger.addBreak(data)
          "null"
    }
    def remove(raw: String): String = {
      decode[Int](raw) match
        case Right(idx)              => debugger.rmBreak(idx)
        case Left(_) if raw == "all" => debugger.rmBreakAll
        case Left(err)               => ??? // TODO handle error
      "null"
    }
    def toggle(raw: String): String = {
      decode[Int](raw) match
        case Right(idx)              => debugger.toggleBreak(idx)
        case Left(_) if raw == "all" => debugger.toggleBreakAll
        case _                       => ??? // TODO handle error
      "null"
    }
  }
}