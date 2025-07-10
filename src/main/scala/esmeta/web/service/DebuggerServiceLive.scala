package esmeta.web.service

import esmeta.cfg.CFG
import esmeta.state.DynamicAddr
import esmeta.web.Debugger
import esmeta.web.http.models
import esmeta.web.util.JsonProtocol
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.std.Semaphore
import cats.syntax.all.*
import io.circe.Json

case class DebuggerServiceLive(
  cfg: CFG,
  debuggerRef: Ref[IO, Option[Debugger]],
  lock: Semaphore[IO],
) extends DebuggerService {

  given JsonProtocol = JsonProtocol(cfg)

  /* accessor */
  private def withDebugger[A](f: Debugger => A): IO[A] =
    debuggerRef.get.flatMap {
      case Some(debugger) => IO.pure(f(debugger))
      case None           => IO.raiseError(ServiceError.DebuggerNotInitialized)
    }

  private def withPermit[A](ioa: IO[A]): IO[A] = lock.permit.use(_ => ioa)

  def addBreak(data: (Boolean, String, List[Int], Boolean)): IO[Boolean] =
    withPermit(withDebugger(_.addBreak(data)))

  def removeBreak(idx: Int): IO[Unit] =
    withPermit(withDebugger(_.rmBreak(idx)))

  def removeAllBreaks: IO[Unit] =
    withPermit(withDebugger(_.rmBreakAll))

  def toggleBreak(idx: Int): IO[Unit] =
    withPermit(withDebugger(_.toggleBreak(idx)))

  def toggleAllBreaks: IO[Unit] =
    withPermit(withDebugger(_.toggleBreakAll))

  def run(body: models.RunRequest): IO[Json] = withPermit {
    val (sourceText, bpData) = body
    val debugger = Debugger(cfg.init.from(sourceText))
    for {
      _ <- bpData.traverse_(data => IO(debugger.addBreak(data)))
      _ <- debuggerRef.set(Some(debugger))
    } yield Debugger.StepResult.ReachedFront
      .withAdditional(debugger, reprint = true)
  }

  def resumeFromIter(body: models.ResumeFromIterRequest): IO[Json] =
    withPermit {
      val (sourceText: String, bpData, iterCount) = body
      val debugger = Debugger(cfg.init.from(sourceText))
      for {
        _ <- bpData.traverse_(data => IO(debugger.addBreak(data)))
        _ <- debuggerRef.set(Some(debugger))
        result = debugger
          .stepExactly(iterCount, true)
          .withAdditional(debugger, reprint = true)
      } yield result
    }

  def backToProvenance(addrStr: String): IO[Json] = withPermit {
    for {
      addrLong <- IO
        .fromOption(addrStr.filter(_.isDigit).toLongOption)(
          ServiceError.InvalidRequest("Invalid address format"),
        )
      addr = DynamicAddr(addrLong)
      result <- withDebugger(d =>
        d.stepBackToProvenance(addr).withAdditional(d),
      )
    } yield result
  }

  def step(
    op: Debugger => Debugger.StepOptions => Debugger.StepResult,
    options: Debugger.StepOptions,
  ): IO[Json] = withPermit {
    withDebugger(d => op(d)(options).withAdditional(d))
  }

  def continue: IO[Json] = withPermit {
    withDebugger(d => d.continue.withAdditional(d))
  }

  def rewind: IO[Json] = withPermit {
    withDebugger(d => d.rewind.withAdditional(d))
  }
}
