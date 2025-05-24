package esmeta.es.util

import esmeta.cfg.*
import esmeta.es.{Ast, Lexical, Syntactic}
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

  given astEncoder: Encoder[Ast] = Encoder.instance {
    case lex @ Lexical(name, str) =>
      Json.obj(
        "Lexical" -> Json.obj(
          "name" -> name.asJson,
          "str" -> str.asJson,
          "loc" -> lex.loc
            .map(loc => (loc.start.offset, loc.end.offset))
            .getOrElse((-1, -1))
            .asJson,
        ),
      )
    case syn @ Syntactic(name, args, rhsIdx, children) =>
      Json.obj(
        "Syntactic" -> Json.obj(
          "name" -> name.asJson,
          "args" -> args.asJson,
          "rhsIdx" -> rhsIdx.asJson,
          "children" -> children.asJson,
          "prodInfo" -> cfg.grammar.prods
            .find(_.name == name)
            .flatMap(_.rhsVec.lift(syn.idx))
            .map(_.getSymbols(syn.rhsIdx))
            .getOrElse(Nil)
            .flatMap(identity)
            .map(sym => (sym.getT, sym.getNt))
            .flatMap {
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
            .asJson,
          "loc" -> syn.loc
            .map(loc => (loc.start.offset, loc.end.offset))
            .getOrElse((-1, -1))
            .asJson,
        ),
      )
  }
}
