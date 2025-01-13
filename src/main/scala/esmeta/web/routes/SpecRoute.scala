package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*
import esmeta.cfg.{CFG, SdoInfo}
import esmeta.spec.{Terminal, Nonterminal}
import esmeta.spec.util.JsonProtocol.given
import esmeta.web.*

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
            cfg.fnameMap
              .flatMap {
                case (name, f) if f.irFunc.algo.isEmpty => None
                case (name, f) =>
                  val algo = f.irFunc.algo.get
                  Some {
                    Json.arr(
                      name.asJson,
                      Json
                        .obj(
                          "name" -> algo.normalizedName.asJson,
                          "htmlId" -> algo.elem.parent().id().asJson,
                          "isBuiltIn" -> f.isBuiltin.asJson,
                          "isSdo" -> f.isSDO.asJson,
                          "sdoInfo" -> f.sdoInfo
                            .map((info: SdoInfo) =>
                              Json
                                .obj(
                                  "method" -> info.method.asJson,
                                  "type" -> (
                                    info match
                                      case _: SdoInfo.Default => "default"
                                      case _: SdoInfo.Base    => "base"
                                  ).asJson,
                                  "prod" -> (
                                    info match
                                      case SdoInfo.Base(_, name, i, j, _) =>
                                        Some(
                                          Json.obj(
                                            "i" -> i.asJson,
                                            "j" -> j.asJson,
                                            "astName" -> name.asJson,
                                            "prodInfo" -> cfg.grammar.prods
                                              .find(_.name == name)
                                              .map { prod =>
                                                val rhs = prod.rhsVec(i)
                                                rhs
                                                  .getSymbols(j)
                                                  .flatMap(x => x)
                                                  .flatMap { symbol =>
                                                    (
                                                      symbol.getT,
                                                      symbol.getNt,
                                                    ) match
                                                      case (Some(t), None) =>
                                                        Some(
                                                          Json.obj(
                                                            "type" -> "terminal".asJson,
                                                            "value" -> t.term.asJson,
                                                          ),
                                                        )
                                                      case (None, Some(nt)) =>
                                                        Some(
                                                          Json.obj(
                                                            "type" -> "nonterminal".asJson,
                                                            "value" -> nt.name.asJson,
                                                          ),
                                                        )
                                                      case _ => None
                                                  }
                                              }
                                              .asJson,
                                          ),
                                        ).asJson // .map(_.asJson),
                                      case _ => None.asJson
                                  ),
                                ),
                            )
                            .asJson,
                        )
                        .asJson,
                    )
                  }
              }
              .toList
              .asJson
              .noSpaces,
          ),
        )
      }
    },
  )
}
