package esmeta.web.service

import cats.effect.IO
import org.http4s.{Response, Status}

sealed trait ServiceError extends Throwable {
  def toResponse: Response[IO]
}

object ServiceError {
  case object DebuggerNotInitialized extends ServiceError {
    val toResponse: Response[IO] = Response(Status.BadRequest)
      .withEntity("Debugger not initialized. Call /run first.")
  }

  case class InvalidRequest(reason: String) extends ServiceError {
    val toResponse: Response[IO] = Response(Status.BadRequest)
      .withEntity(s"Invalid request: $reason")
  }

  case class Internal(cause: Throwable) extends ServiceError {
    println(s"Internal Server Error: ${cause.getMessage}")
    cause.printStackTrace()
    val toResponse: Response[IO] = Response(Status.InternalServerError)
  }
}
