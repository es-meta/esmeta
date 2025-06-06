package esmeta.es.util

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.state.util.{JsonProtocol => StateJsonProtocol}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.dump.DumpSecIdToFuncInfo.convertFuncName

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

  // covered condition metadata
  given coveredCondMetadataDecoder: Decoder[(Cond, (Int, String))] =
    deriveDecoder
  given coveredCondMetadataEncoder: Encoder[(Cond, (Int, String))] =
    Encoder.instance((k, v) =>
      Json.obj(
        "condition" -> k.toString.asJson,
        "location" -> Json.obj(
          "function" -> convertFuncName(cfg.funcOf(cfg.nodeMap(k.id)))(using
            cfg,
          ).asJson,
          "step" -> k.branch.loc
            .flatMap(loc => Some(loc.stepString))
            .getOrElse("NOT_FOUND")
            .asJson,
        ),
        "iter" -> v._1.asJson,
        "script" -> v._2.asJson,
      ),
    )
}
