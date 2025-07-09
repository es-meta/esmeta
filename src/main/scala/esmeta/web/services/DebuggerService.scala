package esmeta.web.services

import esmeta.cfg.CFG
import esmeta.state.DynamicAddr
import esmeta.web.Debugger

import esmeta.web.http.models
import esmeta.web.util.JsonProtocol
import io.circe.*
import zio.*, zio.http.*

trait DebuggerService {

  def addBreak(
    data: (Boolean, String, List[Int], Boolean),
  ): IO[Response, Boolean]
  def removeBreak(idx: Int): IO[Response, Unit]
  def removeAllBreaks: IO[Response, Unit]
  def toggleBreak(idx: Int): IO[Response, Unit]
  def toggleAllBreaks: IO[Response, Unit]

  def run(body: models.RunRequest): IO[Response, Json]
  def resumeFromIter(body: models.ResumeFromIterRequest): IO[Response, Json]
  def backToProvenance(addr: String): IO[Response, Json]

  def step(
    op: Debugger => Debugger.StepOptions => Debugger.StepResult,
    options: Debugger.StepOptions,
  ): IO[Response, Json]

  def continue: IO[Response, Json]
  def rewind: IO[Response, Json]
}

object DebuggerService {
  val layer: ZLayer[CFG, Nothing, DebuggerService] = ZLayer {
    for {
      cfg <- ZIO.service[CFG]
      debuggerRef <- Ref.make[Option[Debugger]](None)
      lock <- Semaphore.make(1)
    } yield DebuggerServiceLive(cfg, debuggerRef, lock)
  }
}
