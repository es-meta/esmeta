package esmeta.es.util

import esmeta.cfg.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ir.EReturnIfAbrupt
import esmeta.state.util.{JsonProtocol => StateJsonProtocol}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends StateJsonProtocol(cfg) {
  import Coverage.*

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
  given viewDecoder: Decoder[View] = optionDecoder
  given viewEncoder: Encoder[View] = optionEncoder
  given nodeViewDecoder: Decoder[NodeView] = deriveDecoder
  given nodeViewEncoder: Encoder[NodeView] = deriveEncoder
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
}
