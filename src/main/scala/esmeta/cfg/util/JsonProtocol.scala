package esmeta.cfg.util
import esmeta.cfg.*
import esmeta.util.*
import io.circe.*, io.circe.syntax.*
import io.circe.generic.auto.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends BasicJsonProtocol {
  // functions
  given funcDecoder: Decoder[Func] = idDecoder(cfg.funcMap.get)
  given funcEncoder: Encoder[Func] = idEncoder

  // nodes
  given nodeDecoder: Decoder[Node] = idDecoder(cfg.nodeMap.get)
  given nodeEncoder: Encoder[Node] = idEncoder

  // block nodes
  given blockDecoder: Decoder[Block] =
    idDecoder(cfg.nodeMap.get(_).collect { case block: Block => block })
  given blockEncoder: Encoder[Block] = idEncoder

  // call nodes
  given callDecoder: Decoder[Call] =
    idDecoder(cfg.nodeMap.get(_).collect { case call: Call => call })
  given callEncoder: Encoder[Call] = idEncoder

  // branch nodes
  given branchDecoder: Decoder[Branch] =
    idDecoder(cfg.nodeMap.get(_).collect { case branch: Branch => branch })
  given branchEncoder: Encoder[Branch] = idEncoder
}
