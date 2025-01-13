package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import esmeta.web.*

/** meta router */
object MetaRoute {
  def apply(): Route = concat(
    path("version") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            esmeta.VERSION.asJson.noSpaces,
          ),
        )
      }
    },
    path("iter") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            _debugger match
              case None => "null"
              case Some(d) => d.getIter.asJson.noSpaces,
          ),
        )
      }
    },
    path("debugString") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            _debugger match
              case None => ("no debugger").toString.asJson.noSpaces
              case Some(d) => {
              val pairs = d.st.context.func.nodes.toList.map(node => node.id.toString() -> d.nodeStepsOpt(node).asJson).toSeq
              Json.obj(pairs*).spaces2
              }                    
          ),
        )
      }
    },
  )
}
