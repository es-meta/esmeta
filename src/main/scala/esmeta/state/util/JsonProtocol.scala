package esmeta.state.util

import esmeta.cfg.*
import esmeta.cfg.util.{JsonProtocol => CFGJsonProtocol}
import esmeta.spec.*
import esmeta.util.*
import esmeta.state.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends CFGJsonProtocol(cfg) {
  // TODO
  // // abstraction of call stacks as simple paths
  // given callPathDecoder: Decoder[CallPath] =
  //   Decoder.instance(
  //     _.value.as[List[Call]].map(calls => CallPath(calls, calls.toSet)),
  //   )

  // given callPathEncoder: Encoder[CallPath] =
  //   Encoder.instance(cp => Json.fromValues(cp.path.map(_.asJson)))

  // // ECMAScript features
  // given featureDecoder: Decoder[Feature] =
  //   Decoder.instance(c =>
  //     for {
  //       func <- funcDecoder(c)
  //       feature <- func.head match
  //         case Some(head: SyntaxDirectedOperationHead) =>
  //           Right(SyntacticFeature(func, head))
  //         case Some(head: BuiltinHead) =>
  //           Right(BuiltinFeature(func, head))
  //         case _ =>
  //           invalidFail("feature", c)
  //     } yield feature,
  //   )
  // given featureEncoder: Encoder[Feature] =
  //   Encoder.instance(f => funcEncoder(f.func))
}
