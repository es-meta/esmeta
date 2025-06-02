package esmeta.web.util

import esmeta.cfg.*
import esmeta.es.{Ast, Hole, Lexical, Syntactic}
import esmeta.ir.{Func as IRFunc, FuncKind}
import esmeta.util.*
import esmeta.util.BasicJsonProtocol
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: esmeta.cfg.CFG) extends BasicJsonProtocol {

  // sdo info
  given sdoInfoEncoder: Encoder[SdoInfo] = Encoder.instance { (info: SdoInfo) =>
    info match
      case SdoInfo.Default(func, method) =>
        Json.obj(
          "method" -> info.method.asJson,
          "type" -> "default".asJson,
          "prod" -> None.asJson,
        )
      case SdoInfo.Base(func, name, i, j, method) =>
        Json.obj(
          "method" -> info.method.asJson,
          "type" -> "base".asJson,
          "prod" -> Json.obj(
            "i" -> i.asJson,
            "j" -> j.asJson,
            "astName" -> name.asJson,
            "prodInfo" -> cfg.grammar.prods
              .find(_.name == name)
              .flatMap(_.rhsVec.lift(i))
              .map(_.getSymbols(j))
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
          ),
        )
  }

  /** Mapping of IRFunc to Metalang metadata */
  val funcToSpecInfoEncoder: Encoder[Func] = Encoder.instance { (f: Func) =>
    f.irFunc.algo match
      case None => Json.Null
      case Some(algo) =>
        Json
          .obj(
            "normalizedName" -> algo.normalizedName.asJson,
            "name" -> algo.name.asJson,
            "htmlId" -> algo.elem.parent().id().asJson,
            "isBuiltIn" -> f.isBuiltin.asJson,
            "isSdo" -> f.isSDO.asJson,
            "isClo" -> f.isClo.asJson,
            "isCont" -> f.isCont.asJson,
            "sdoInfo" -> f.sdoInfo.asJson,
            "isMethod" -> f.isMethod.asJson,
            "methodInfo" -> f.irFunc.methodName.asJson,
          )
  }

  val cfgToFuncEncoder: Encoder[CFG] = Encoder.instance { (cfg: CFG) =>
    cfg.fnameMap
      .map {
        case (name, f) =>
          (
            f.id,
            (
              f.name,
              f.kind.ordinal,
              f.params.map((p: esmeta.ir.Param) =>
                (
                  p.lhs.name,
                  p.optional,
                  p.ty.toString,
                ),
              ),
              f.irFunc.algo
                .map(_.code)
                .getOrElse(""),
              f.asJson(using funcToSpecInfoEncoder),
            ),
          )
      }
      .toList
      .asJson
  }

  given astEncoder: Encoder[Ast] = Encoder.instance {
    case _: Hole => ???
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

  /* auxiliaries for encoding */

  extension (irFunc: IRFunc) {

    def methodNameRaw: Option[(String, String)] = MethodNameParser.parseAll(
      MethodNameParser.funcName,
      irFunc.name,
    ) match {
      case MethodNameParser.Success(result, _) => Some(result)
      case _                                   => None
    }

    def methodName = irFunc.kind match
      case FuncKind.InternalMeth =>
        irFunc.methodNameRaw.map((t, n) => (t, s"[[${n}]]"))
      case FuncKind.ConcMeth => irFunc.methodNameRaw
      case _                 => None

  }

  private object MethodNameParser extends BasicParsers {
    // override protected val whiteSpace: Regex = "".r
    lazy val funcName = ("Record[" ~> "\\w+".r <~ "]") ~ ("." ~> "\\w+".r) ^^ {
      case t ~ n => (t, n)
    }
  }

}
