package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*
import esmeta.cfg.{CFG, SdoInfo}
import esmeta.cfg.util.{JsonProtocol as CFGJsonProtocol}
import esmeta.ir.{Func, FuncKind}
import esmeta.ir.util.{Parser}
import esmeta.spec.{Terminal, Nonterminal}
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.{BasicParsers}
import esmeta.web.*
import scala.util.Try

/** spec router */
object SpecRoute {

  /** root router */
  def apply(cfg: CFG): Route = concat(
    path("func") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            cfg.fnameMap
              .map { case (name, f) => (f.id, name) }
              .toList
              .asJson
              .noSpaces,
          ),
        )
      }
    },
    path("version") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            cfg.spec.version.asJson.noSpaces,
          ),
        )
      }
    },
    path("irToSpecNameMap") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            cfg
              .asJson(using CFGJsonProtocol(cfg).irToSpecNameMapEncoder)
              .noSpaces,
          ),
        )
      }
    },
  )
}
