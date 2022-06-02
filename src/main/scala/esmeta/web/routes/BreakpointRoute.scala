package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import esmeta.web.*

/** breakpoint router */
object BreakpointRoute {
  // root router
  def apply(): Route = pathEnd {
    concat(
      // add breakpoint
      // TODO add steps
      post {
        entity(as[String]) { raw =>
          decode[(Boolean, Int, List[Int], Boolean)](raw) match
            case Left(err) => ??? // TODO handle error
            case Right(data) =>
              println(data)
              debugger.addBreak(data)
              complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
      // remove breakpoint
      delete {
        entity(as[String]) { raw =>
          decode[Int](raw) match
            case Right(idx)              => debugger.rmBreak(idx)
            case Left(_) if raw == "all" => debugger.rmBreakAll
            case Left(err)               => ??? // TODO handle error
          complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
      // toggle breakpoint
      put {
        entity(as[String]) { raw =>
          decode[Int](raw) match
            case Right(idx)              => debugger.toggleBreak(idx)
            case Left(_) if raw == "all" => debugger.toggleBreakAll
            case _                       => ??? // TODO handle error
          complete(HttpEntity(ContentTypes.`application/json`, "null"))
        }
      },
    )
  }
}
