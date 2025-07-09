package esmeta.web.services

import esmeta.cfg.CFG
import esmeta.state.DynamicAddr
import esmeta.web.Debugger

import esmeta.web.http.models
import esmeta.web.util.JsonProtocol
import io.circe.*
import zio.*, zio.http.*

case class DebuggerServiceLive(
  cfg: CFG,
  debuggerRef: Ref[Option[Debugger]],
  lock: Semaphore,
) extends DebuggerService {

  given JsonProtocol = JsonProtocol(cfg)

  /* safe accessor */
  private def withDebugger[A](f: Debugger => A): IO[Response, A] =
    debuggerRef.get.flatMap {
      case Some(debugger) => ZIO.succeed(f(debugger))
      case None =>
        ZIO.fail(
          Response.badRequest("Debugger not initialized. Call /run first."),
        )
    }

  def addBreak(
    data: (Boolean, String, List[Int], Boolean),
  ): IO[Response, Boolean] =
    lock.withPermit(withDebugger(_.addBreak(data)))

  def removeBreak(idx: Int): IO[Response, Unit] =
    lock.withPermit(withDebugger(_.rmBreak(idx)))

  def removeAllBreaks: IO[Response, Unit] =
    lock.withPermit(withDebugger(_.rmBreakAll))

  def toggleBreak(idx: Int): IO[Response, Unit] =
    lock.withPermit(withDebugger(_.toggleBreak(idx)))

  def toggleAllBreaks: IO[Response, Unit] =
    lock.withPermit(withDebugger(_.toggleBreakAll))

  def run(
    body: models.RunRequest,
  ): IO[Response, Json] = lock.withPermit {
    val (sourceText, bpData) = body
    val debugger = Debugger(cfg.init.from(sourceText))
    for {
      _ <- ZIO.foreachDiscard(bpData)(data =>
        ZIO.succeed(debugger.addBreak(data)),
      )
      _ <- debuggerRef.set(Some(debugger))
    } yield Debugger.StepResult.ReachedFront
      .withAdditional(debugger, reprint = true)
  }

  def resumeFromIter(body: models.ResumeFromIterRequest): IO[Response, Json] =
    lock.withPermit {
      val (sourceText: String, bpData, iterCount) = body
      val debugger = Debugger(cfg.init.from(sourceText))
      for {
        _ <- ZIO.foreachDiscard(bpData)(data =>
          ZIO.succeed(debugger.addBreak(data)),
        )
        _ <- debuggerRef.set(Some(debugger))
        result = debugger
          .stepExactly(iterCount, true)
          .withAdditional(debugger, reprint = true)
      } yield result
    }

  def backToProvenance(addrStr: String): IO[Response, Json] = lock.withPermit {
    for {
      addrLong <- ZIO
        .fromOption(addrStr.filter(_.isDigit).toLongOption)
        .mapError(_ => Response.badRequest("Invalid address format"))
      addr = DynamicAddr(addrLong)
      result <- withDebugger(d =>
        d.stepBackToProvenance(addr).withAdditional(d),
      )
    } yield result
  }

  def step(
    op: Debugger => Debugger.StepOptions => Debugger.StepResult,
    options: Debugger.StepOptions,
  ): IO[Response, Json] = lock.withPermit {
    withDebugger(d => op(d)(options).withAdditional(d))
  }

  def continue: IO[Response, Json] = lock.withPermit {
    withDebugger(d => d.continue.withAdditional(d))
  }

  def rewind: IO[Response, Json] = lock.withPermit {
    withDebugger(d => d.rewind.withAdditional(d))
  }
}
