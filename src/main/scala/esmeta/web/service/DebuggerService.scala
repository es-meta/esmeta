package esmeta.web.service

import esmeta.cfg.CFG
import esmeta.state.DynamicAddr
import esmeta.web.Debugger
import esmeta.web.http.models
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import io.circe.Json

trait DebuggerService {
  def addBreak(data: (Boolean, String, List[Int], Boolean)): IO[Boolean]
  def removeBreak(idx: Int): IO[Unit]
  def removeAllBreaks: IO[Unit]
  def toggleBreak(idx: Int): IO[Unit]
  def toggleAllBreaks: IO[Unit]

  def run(body: models.RunRequest): IO[Json]
  def resumeFromIter(body: models.ResumeFromIterRequest): IO[Json]
  def backToProvenance(addr: String): IO[Json]

  def step(
    op: Debugger => Debugger.StepOptions => Debugger.StepResult,
    options: Debugger.StepOptions,
  ): IO[Json]

  def continue: IO[Json]
  def rewind: IO[Json]
}

object DebuggerService {
  def of(cfg: CFG): IO[DebuggerService] =
    for {
      debuggerRef <- Ref.of[IO, Option[Debugger]](None)
      lock <- Semaphore[IO](1)
    } yield DebuggerServiceLive(cfg, debuggerRef, lock)
}
