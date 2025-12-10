package esmeta.es.util

import esmeta.cfg.*
import esmeta.es.*
import esmeta.state.util.{JsonProtocol as StateJsonProtocol}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends StateJsonProtocol(cfg) {
  import Coverage.*

  given viewDecoder: Decoder[View] = optionDecoder
  given viewEncoder: Encoder[View] =
    Encoder.instance {
      case None => Json.Null
      case Some((enclosing, feature, path)) =>
        Json.obj(
          "enclosing" -> Json.fromValues(
            enclosing.map(f => f.func.name.asJson),
          ),
          "feature" -> feature.func.name.asJson,
          "path" -> path.map(_.toString).asJson,
        )
    }

  given nodeViewDecoder: Decoder[NodeView] = deriveDecoder
  given nodeViewEncoder: Encoder[NodeView] =
    Encoder.instance(nv =>
      Json.obj(
        "node" -> Json.obj(
          "name" -> nv.node.name.asJson,
          "inst" -> nv.node.toString.asJson,
          "func" -> cfg.funcOf(nv.node).name.asJson,
        ),
        "view" -> viewEncoder(nv.view),
      ),
    )
  given condViewDecoder: Decoder[CondView] = deriveDecoder
  given condViewEncoder: Encoder[CondView] = deriveEncoder
  given funcViewDecoder: Decoder[FuncView] = deriveDecoder
  given funcViewEncoder: Encoder[FuncView] = deriveEncoder

  // conditions
  given condDecoder: Decoder[Cond] = deriveDecoder
  given condEncoder: Encoder[Cond] = deriveEncoder

  // meta-info for each view or features
  given nodeViewInfoDecoder: Decoder[NodeViewInfo] = deriveDecoder
  given nodeViewInfoEncoder: Encoder[NodeViewInfo] = deriveEncoder
  given condViewInfoDecoder: Decoder[CondViewInfo] = deriveDecoder
  given condViewInfoEncoder: Encoder[CondViewInfo] = deriveEncoder

  // coverage constructor
  given coverageConstructorDecoder: Decoder[CoverageConstructor] = deriveDecoder
  given coverageConstructorEncoder: Encoder[CoverageConstructor] = deriveEncoder

  // code object
  import Code.*
  given Decoder[Normal] = deriveDecoder
  given Encoder[Normal] = deriveEncoder
  given Decoder[Builtin] = deriveDecoder
  given Encoder[Builtin] = deriveEncoder

  given codeDecoder: Decoder[Code] = Decoder.instance { c =>
    c.get[String]("kind").flatMap {
      case "Normal" =>
        c.get[Normal]("info").map { info => Normal(info.sourceText) }
      case "Builtin" =>
        c.get[Builtin]("info").map { info =>
          Builtin(
            func = info.func,
            thisArg = info.thisArg,
            args = info.args,
            preStmts = info.preStmts,
            postStmts = info.postStmts,
          )
        }
    }
  }
  given codeEncoder: Encoder[Code] = Encoder.instance { code =>
    code match
      case Normal(sourceText) =>
        Json.obj(
          "kind" -> "Normal".asJson,
          "info" -> Json.obj("sourceText" -> sourceText.asJson),
        )
      case Builtin(func, thisArg, args, preStmts, postStmts) =>
        Json.obj(
          "kind" -> "Builtin".asJson,
          "info" -> Json.obj(
            "func" -> func.asJson,
            "thisArg" -> thisArg.asJson,
            "args" -> args.asJson,
            "preStmts" -> preStmts.asJson,
            "postStmts" -> postStmts.asJson,
          ),
        )
  }
}
