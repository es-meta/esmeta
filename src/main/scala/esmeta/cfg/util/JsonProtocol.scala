package esmeta.cfg.util
import esmeta.cfg.*
import esmeta.ir.{Func as IRFunc, FuncKind}
import esmeta.util.*
import io.circe.*, io.circe.syntax.*
import io.circe.generic.auto.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends BasicJsonProtocol {
  // functions
  given funcDecoder: Decoder[Func] = uidDecoder(cfg.funcMap.get)
  given funcEncoder: Encoder[Func] = uidEncoder

  // nodes
  given nodeDecoder: Decoder[Node] = uidDecoder(cfg.nodeMap.get)
  given nodeEncoder: Encoder[Node] = uidEncoder

  // block nodes
  given blockDecoder: Decoder[Block] =
    uidDecoder(cfg.nodeMap.get(_).collect { case block: Block => block })
  given blockEncoder: Encoder[Block] = uidEncoder

  // call nodes
  given callDecoder: Decoder[Call] =
    uidDecoder(cfg.nodeMap.get(_).collect { case call: Call => call })
  given callEncoder: Encoder[Call] = uidEncoder

  // branch nodes
  given branchDecoder: Decoder[Branch] =
    uidDecoder(cfg.nodeMap.get(_).collect { case branch: Branch => branch })
  given branchEncoder: Encoder[Branch] = uidEncoder

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

  /** Mapping of IRFunc to Metalang code */
  val irFuncToCode: Encoder[CFG] = Encoder.instance { (cfg: CFG) =>
    Map
      .from(
        cfg.program.funcs.map { f =>
          f.name -> f.algo.map(_.code)
        },
      )
      .asJson
  }

  /** Mapping of IRFunc to Metalang metadata */
  val irToSpecNameMapEncoder: Encoder[CFG] = Encoder.instance { (cfg: CFG) =>
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
                  "sdoInfo" -> f.sdoInfo.asJson,
                  "isMethod" -> f.isMethod.asJson,
                  "sdoInfo" -> f.sdoInfo.asJson,
                  "methodInfo" -> f.irFunc.methodName.asJson,
                )
                .asJson,
            )
          }
      }
      .toList
      .asJson
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
