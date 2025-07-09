package esmeta.web.http.routes

import esmeta.web.*
import esmeta.web.http.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import zio.*
import zio.http.*

object MetaRoute extends ZIOAppDefault {

  val routes =
    Routes(
      Method.GET / "version" -> handler { (req: Request) =>
        val name = req.queryOrElse[String]("name", "World")
        esmeta.VERSION.asJsonResponse
      },
    )

  def run = Server.serve(routes).provide(Server.default)
}
