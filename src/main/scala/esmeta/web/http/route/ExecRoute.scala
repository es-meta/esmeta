package esmeta.web.http.route

import esmeta.web.Debugger
import esmeta.web.http.*
import esmeta.web.service.*
import esmeta.web.util.JsonProtocol
import cats.effect.IO, cats.syntax.all.*
import io.circe.*, io.circe.generic.auto.*, io.circe.syntax.*
import io.circe.parser.decode
import org.http4s.*, org.http4s.dsl.io.*, org.http4s.headers.`Content-Type`

class ExecRoute(service: DebuggerService)(using JsonProtocol) {

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

  private def jsonResponse(json: Json): IO[Response[IO]] =
    Ok(json.noSpaces, `Content-Type`(MediaType.application.json))

  private def handleErrors(io: IO[Json]): IO[Response[IO]] =
    io.flatMap(jsonResponse).handleErrorWith {
      case se: ServiceError => IO.pure(se.toResponse)
      case e: Throwable     => IO.pure(ServiceError.Internal(e).toResponse)
    }

  private def stepHandler(
    serviceCall: (DebuggerService, Debugger.StepOptions) => IO[Json],
  ): Request[IO] => IO[Response[IO]] = { req =>
    val result = for {
      ignoreBreak <- decodeJson[Boolean](req)
      options = Debugger.StepOptions(ignoreBreak)
      json <- serviceCall(service, options)
    } yield json
    handleErrors(result)
  }

  private def simpleHandler(
    serviceCall: DebuggerService => IO[Json],
  ): Request[IO] => IO[Response[IO]] = { _ =>
    handleErrors(serviceCall(service))
  }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "run" =>
      handleErrors(decodeJson[models.RunRequest](req).flatMap(service.run))

    case req @ POST -> Root / "backToProvenance" =>
      handleErrors(req.as[String].flatMap(service.backToProvenance))

    case req @ POST -> Root / "resumeFromIter" =>
      handleErrors(
        decodeJson[models.ResumeFromIterRequest](req)
          .flatMap(service.resumeFromIter),
      )

    // spec steps
    case req @ POST -> Root / "specStep" =>
      stepHandler(_.step(_.specStep, _))(req)
    case req @ POST -> Root / "specStepOver" =>
      stepHandler(_.step(_.specStepOver, _))(req)
    case req @ POST -> Root / "specStepOut" =>
      stepHandler(_.step(_.specStepOut, _))(req)
    case req @ POST -> Root / "specStepBack" =>
      stepHandler(_.step(_.specStepBack, _))(req)
    case req @ POST -> Root / "specStepBackOver" =>
      stepHandler(_.step(_.specStepBackOver, _))(req)
    case req @ POST -> Root / "specStepBackOut" =>
      stepHandler(_.step(_.specStepBackOut, _))(req)

    // continue / rewind
    case req @ POST -> Root / "specContinue" => simpleHandler(_.continue)(req)
    case req @ POST -> Root / "specRewind"   => simpleHandler(_.rewind)(req)

    // ir steps
    case req @ POST -> Root / "irStep" => stepHandler(_.step(_.irStep, _))(req)
    case req @ POST -> Root / "irStepOver" =>
      stepHandler(_.step(_.irStepOver, _))(req)
    case req @ POST -> Root / "irStepOut" =>
      stepHandler(_.step(_.irStepOut, _))(req)
    case req @ POST -> Root / "stepCntPlus" =>
      stepHandler(_.step(_.stepCntPlus, _))(req)
    case req @ POST -> Root / "stepCntMinus" =>
      stepHandler(_.step(_.stepCntMinus, _))(req)
    case req @ POST -> Root / "instCntPlus" =>
      stepHandler(_.step(_.instCntPlus, _))(req)
    case req @ POST -> Root / "instCntMinus" =>
      stepHandler(_.step(_.instCntMinus, _))(req)

    // es steps
    case req @ POST -> Root / "esAstStep" =>
      stepHandler(_.step(_.esAstStep, _))(req)
    case req @ POST -> Root / "esStatementStep" =>
      stepHandler(_.step(_.esStatementStep, _))(req)
    case req @ POST -> Root / "esStepOver" =>
      stepHandler(_.step(_.esStepOver, _))(req)
    case req @ POST -> Root / "esStepOut" =>
      stepHandler(_.step(_.esStepOut, _))(req)
  }
}
