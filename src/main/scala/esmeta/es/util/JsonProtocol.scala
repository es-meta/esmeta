package esmeta.es.util

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ir.EReturnIfAbrupt
import esmeta.state.util.{JsonProtocol => StateJsonProtocol}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends StateJsonProtocol(cfg) {
  import Coverage.*
  import CoverageSmall.{ViewSmall, NodeViewSmall, CondViewSmall}

  // branch or reference to EReturnIfAbrupt with boolean values
  given condDecoder: Decoder[Cond] = deriveDecoder
  given condEncoder: Encoder[Cond] = deriveEncoder
  given condElemDecoder: Decoder[Branch | WeakUIdRef[EReturnIfAbrupt]] =
    val branchDecoder = idDecoder[Branch | WeakUIdRef[EReturnIfAbrupt]](
      "branch",
      cfg.nodeMap.get(_).collect { case branch: Branch => branch },
    )
    val abruptDecoder = idDecoder[Branch | WeakUIdRef[EReturnIfAbrupt]](
      "abrupt",
      cfg.riaExprMap.get(_).map(_.idRef),
    )
    branchDecoder.handleErrorWith(_ => abruptDecoder)
  given condElemEncoder: Encoder[Branch | WeakUIdRef[EReturnIfAbrupt]] =
    Encoder.instance {
      case branch: Branch                      => idEncoder("branch")(branch)
      case abrupt: WeakUIdRef[EReturnIfAbrupt] => idEncoder("abrupt")(abrupt)
    }

  // syntax-sensitive views
  // given viewDecoder: Decoder[View] = optionDecoder
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
  // given nodeViewDecoder: Decoder[NodeView] = deriveDecoder
  given nodeViewEncoder: Encoder[NodeView] =
    Encoder.instance(nv =>
      Json.obj(
        "node" -> Json.obj(
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

  // syntax-sensitive views with small information
  // given viewSmallDecoder: Decoder[ViewSmall] = optionDecoder
  given viewSmallEncoder: Encoder[ViewSmall] =
    Encoder.instance {
      case None => Json.Null
      case Some((enclosing, feature, path)) =>
        Json.obj(
          "enclosing" -> Json.fromValues(
            enclosing.map(f => f.func.id.asJson),
          ),
          "feature" -> feature.func.id.asJson,
          "path" -> path.map(_.toString).asJson, // to be changed
        )
    }
  given viewSmallDecoder: Decoder[ViewSmall] = deriveDecoder

  // given nodeViewSmallDecoder: Decoder[NodeViewSmall] = deriveDecoder
  given nodeViewSmallEncoder: Encoder[NodeViewSmall] =
    Encoder.instance(nv =>
      Json.obj(
        "node" -> Json.obj(
          "inst" -> nv.node.id.asJson,
          "func" -> cfg.funcOf(nv.node).id.asJson,
        ),
        "view" -> viewSmallEncoder(nv.view),
      ),
    )

  given condViewSmallDecoder: Decoder[CondViewSmall] = deriveDecoder
  given condViewSmallEncoder: Encoder[CondViewSmall] = deriveEncoder
}
