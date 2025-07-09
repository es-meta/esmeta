// esmeta/web/http/routes/BreakpointRoute.scala
package esmeta.web.http.routes

import esmeta.web.http.models.AddBreakpointRequest
import esmeta.web.services.DebuggerService
import zio.*, zio.http.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*
import io.circe.parser.decode

object BreakpointRoute {
  def apply(): Routes[DebuggerService, Response] =
    Routes(
      Method.POST / Root -> handler { (req: Request) =>
        for {
          body <- req.body.asString.flatMap { rawString =>
            ZIO
              .fromEither(decode[AddBreakpointRequest](rawString))
              .mapError(err =>
                Response.badRequest(s"Invalid JSON: ${err.getMessage}"),
              )
          }
          service <- ZIO.service[DebuggerService]
          newBreaks <- service.addBreak(body)
        } yield Response.json(newBreaks.asJson.noSpaces)
      },
      Method.DELETE / Root -> handler { (req: Request) =>
        for {
          bodyStr <- req.body.asString
          service <- ZIO.service[DebuggerService]
          _ <- bodyStr match {
            case "all" =>
              service.removeAllBreaks
            case _ =>
              ZIO
                .fromOption(bodyStr.toIntOption)
                .mapError(_ =>
                  Response.badRequest("Body must be an integer index or 'all'"),
                )
                .flatMap(service.removeBreak)
          }
        } yield Response.ok
      },
      Method.PUT / Root -> handler { (req: Request) =>
        for {
          bodyStr <- req.body.asString
          service <- ZIO.service[DebuggerService]
          _ <- bodyStr match {
            case "all" =>
              service.toggleAllBreaks
            case _ =>
              ZIO
                .fromOption(bodyStr.toIntOption)
                .mapError(_ =>
                  Response.badRequest("Body must be an integer index or 'all'"),
                )
                .flatMap(service.toggleBreak)
          }
        } yield Response.ok
      },
    ).handleError(_ => Response.internalServerError)
}
