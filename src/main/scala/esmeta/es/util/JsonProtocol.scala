package esmeta.es.util

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.state.util.{JsonProtocol => StateJsonProtocol}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends StateJsonProtocol(cfg) {
  import Coverage.*

  given tmpNodeDecoder: Decoder[TmpNode] = deriveDecoder
  given tmpViewDecoder: Decoder[TmpView] = deriveDecoder
  given tmpNodeViewDecoder: Decoder[TmpNodeView] = deriveDecoder
  given tmpNodeViewInfoDecoder: Decoder[TmpNodeViewInfo] = deriveDecoder

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

  // meta-info for each view or features
  given nodeViewInfoDecoder: Decoder[NodeViewInfo] = deriveDecoder
  given nodeViewInfoEncoder: Encoder[NodeViewInfo] = deriveEncoder
  given condViewInfoDecoder: Decoder[CondViewInfo] = deriveDecoder
  given condViewInfoEncoder: Encoder[CondViewInfo] = deriveEncoder

  // coverage constructor
  given coverageConstructorDecoder: Decoder[CoverageConstructor] = deriveDecoder
  given coverageConstructorEncoder: Encoder[CoverageConstructor] = deriveEncoder

  case class TmpView(
    enclosing: List[String],
    feature: String,
    path: String,
  )

  case class TmpNode(
    name: String,
    inst: String,
    func: String,
  )

  case class TmpNodeView(
    node: TmpNode,
    view: Option[TmpView],
  )

  case class TmpNodeViewInfo(
    index: Int,
    nodeView: TmpNodeView,
    script: String,
  )
}
