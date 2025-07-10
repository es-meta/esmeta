package esmeta.web.http.route

import esmeta.web.http.*
import esmeta.web.service.*
import cats.effect.IO, cats.syntax.all.*
import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*
import io.circe.parser.decode
import org.http4s.*, org.http4s.dsl.io.*

class BreakpointRoute(service: DebuggerService) {
  private def decodeJson[A: Decoder](req: Request[IO]): IO[A] =
    req
      .as[String]
      .flatMap(rawBody =>
        IO.fromEither(decode[A](rawBody))
          .adaptError {
            case e: ParsingFailure =>
              ServiceError.InvalidRequest(s"Invalid JSON: ${e.message}")
          },
      )

  private def handleErrors(program: IO[Response[IO]]): IO[Response[IO]] =
    program.handleErrorWith {
      case se: ServiceError => IO.pure(se.toResponse)
      case e: Throwable     => IO.pure(ServiceError.Internal(e).toResponse)
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ POST -> Root =>
      handleErrors {
        for {
          body <- decodeJson[models.AddBreakpointRequest](req)
          bool <- service.addBreak(body)
          resp <- bool.asJsonOk
        } yield resp
      }

    case req @ DELETE -> Root =>
      handleErrors {
        for {
          body <- req.as[String]
          bool <- body.trim match {
            case "all" => service.removeAllBreaks
            case str =>
              IO.fromOption(str.toIntOption)(
                ServiceError.InvalidRequest(
                  "Body must be an integer index or 'all'",
                ),
              ).flatMap(service.removeBreak)
          }
          resp <- bool.asJsonOk
        } yield resp
      }

    case req @ PUT -> Root =>
      handleErrors {
        for {
          body <- req.as[String]
          bool <- body.trim match {
            case "all" => service.toggleAllBreaks
            case str =>
              IO.fromOption(str.toIntOption)(
                ServiceError.InvalidRequest(
                  "Body must be an integer index or 'all'",
                ),
              ).flatMap(service.toggleBreak)
          }
          resp <- bool.asJsonOk
        } yield resp
      }
  }
}
