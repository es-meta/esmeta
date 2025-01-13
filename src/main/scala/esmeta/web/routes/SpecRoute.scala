package esmeta.web.routes

import akka.http.scaladsl.model.*
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import io.circe.*, io.circe.syntax.*
import esmeta.cfg.{CFG, SdoInfo}
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
                          // name is unused
                          "name" -> algo.normalizedName.asJson,
                          "htmlId" -> algo.elem.parent().id().asJson,
                          "isBuiltIn" -> f.isBuiltin.asJson,
                          "isSdo" -> f.isSDO.asJson,
                          "sdoInfo" -> f.sdoInfo.asJson(using
                            SdoInfoJsonProtocol.encoder(using cfg),
                          ),
                          "isMethod" -> f.isMethod.asJson,
                          "sdoInfo" -> f.sdoInfo.asJson(using
                            SdoInfoJsonProtocol.encoder(using cfg),
                          ),
                          "methodInfo" -> f.irFunc.methodName.asJson,
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

object SdoInfoJsonProtocol {
  inline def encoder(using cfg: CFG): Encoder[Option[SdoInfo]] =
    Encoder.instance { opt =>
      opt
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
        .asJson
    }
}

extension (func: Func) {

  def methodNameRaw: Option[(String, String)] = MethodNameParser.parseAll(
    MethodNameParser.funcName,
    func.name,
  ) match {
    case MethodNameParser.Success(result, _) => Some(result)
    case _                                   => None
  }

  def methodName = func.kind match
    case FuncKind.InternalMeth =>
      func.methodNameRaw.map((t, n) => (t, s"[[${n}]]"))
    case FuncKind.ConcMeth => func.methodNameRaw
    case _                 => None
}

private object MethodNameParser extends BasicParsers {
  // override protected val whiteSpace: Regex = "".r
  lazy val funcName = ("Record[" ~> "\\w+".r <~ "]") ~ ("." ~> "\\w+".r) ^^ {
    case t ~ n => (t, n)
  }
}
