package esmeta.web.http.route

import esmeta.web.*
import esmeta.web.http.*
import cats.effect.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import org.http4s.dsl.io.*
import org.http4s.{HttpApp, HttpRoutes}

object MetaRoute {
  def apply() = HttpRoutes.of[IO] {
    case GET -> Root / "version" =>
      esmeta.VERSION.asJsonOk
  }
}
