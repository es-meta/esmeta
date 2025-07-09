// esmeta/web/http/routes/ExecRoute.scala
package esmeta.web.http.routes

import esmeta.web.Debugger
import esmeta.web.Debugger.*
import esmeta.web.http.*
import esmeta.web.services.DebuggerService
import esmeta.web.util.JsonProtocol
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*
import io.circe.parser.decode
import zio.*, zio.http.*

object ExecRoute {

  private def stepHandler(
    serviceCall: (DebuggerService, Debugger.StepOptions) => IO[Response, Json],
  ): Handler[DebuggerService, Response, Request, Response] = {
    handler { (req: Request) =>
      for {
        rawBody <- req.body.asString.mapError(err =>
          Response.badRequest(
            s"Invalid JSON for step option: ${err.getMessage}",
          ),
        )
        ignoreBreak <- ZIO
          .fromEither(decode[Boolean](rawBody))
          .mapError(err =>
            Response.badRequest(
              s"Invalid JSON for step option: ${err.getMessage}",
            ),
          )
        options = Debugger.StepOptions(ignoreBreak)
        service <- ZIO.service[DebuggerService]
        result <- serviceCall(service, options)
      } yield Response.json(result.noSpaces)
    }
  }

  private def simpleHandler(
    serviceCall: DebuggerService => IO[Response, Json],
  ): Handler[DebuggerService, Response, Request, Response] = {
    handler { (_: Request) =>
      for {
        service <- ZIO.service[DebuggerService]
        result <- serviceCall(service)
      } yield Response.json(result.noSpaces)
    }
  }

  def apply()(using JsonProtocol): Routes[DebuggerService, Response] = Routes(
    Method.POST / "run" -> handler { (req: Request) =>
      for {
        body <- req.body.asString.flatMap { rawString =>
          ZIO
            .fromEither(decode[models.RunRequest](rawString))
            .mapError(err =>
              Response.badRequest(s"Invalid body: ${err.getMessage}"),
            )
        }
        service <- ZIO.service[DebuggerService]
        result <- service.run(body)
      } yield Response.json(result.asJson.noSpaces)
    },
    Method.POST / "backToProvenance" -> handler { (req: Request) =>
      for {
        addr <- req.body.asString.mapError(e =>
          Response.badRequest(s"Invalid body: $e"),
        )
        service <- ZIO.service[DebuggerService]
        result <- service.backToProvenance(addr)
      } yield Response.json(result.asJson.noSpaces)
    },
    Method.POST / "resumeFromIter" -> handler { (req: Request) =>
      for {
        body <- req.body.asString.flatMap { rawString =>
          ZIO
            .fromEither(decode[models.ResumeFromIterRequest](rawString))
            .mapError(err =>
              Response.badRequest(s"Invalid body: ${err.getMessage}"),
            )
        }
        service <- ZIO.service[DebuggerService]
        result <- service.resumeFromIter(body)
      } yield Response.json(result.asJson.noSpaces)
    },

    // spec steps
    Method.POST / "specStep" -> stepHandler(_.step(_.specStep, _)),
    Method.POST / "specStepOver" -> stepHandler(_.step(_.specStepOver, _)),
    Method.POST / "specStepOut" -> stepHandler(_.step(_.specStepOut, _)),
    Method.POST / "specStepBack" -> stepHandler(_.step(_.specStepBack, _)),
    Method.POST / "specStepBackOver" -> stepHandler(
      _.step(_.specStepBackOver, _),
    ),
    Method.POST / "specStepBackOut" -> stepHandler(
      _.step(_.specStepBackOut, _),
    ),

    // simple continue/rewind
    Method.POST / "specContinue" -> simpleHandler(_.continue),
    Method.POST / "specRewind" -> simpleHandler(_.rewind),

    // ir steps
    Method.POST / "irStep" -> stepHandler(_.step(_.irStep, _)),
    Method.POST / "irStepOver" -> stepHandler(_.step(_.irStepOver, _)),
    Method.POST / "irStepOut" -> stepHandler(_.step(_.irStepOut, _)),
    Method.POST / "stepCntPlus" -> stepHandler(_.step(_.stepCntPlus, _)),
    Method.POST / "stepCntMinus" -> stepHandler(_.step(_.stepCntMinus, _)),
    Method.POST / "instCntPlus" -> stepHandler(_.step(_.instCntPlus, _)),
    Method.POST / "instCntMinus" -> stepHandler(_.step(_.instCntMinus, _)),

    // ES steps
    Method.POST / "esAstStep" -> stepHandler(_.step(_.esAstStep, _)),
    Method.POST / "esStatementStep" -> stepHandler(
      _.step(_.esStatementStep, _),
    ),
    Method.POST / "esStepOver" -> stepHandler(_.step(_.esStepOver, _)),
    Method.POST / "esStepOut" -> stepHandler(_.step(_.esStepOut, _)),
  ).handleError(_ => Response.internalServerError)
}
