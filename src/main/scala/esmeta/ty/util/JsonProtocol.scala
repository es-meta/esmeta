package esmeta
package ty
package util

import esmeta.lang.Syntax
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.spec
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.spec.Spec

object JsonProtocol extends BasicJsonProtocol {
  private val stringifier = TyElem.getStringifier(false, false)
  import stringifier.given

  given Decoder[BoolTy] = decoderWithParser(BoolTy.from)
  given Encoder[BoolTy] = encoderWithStringifier(stringify)
  given Decoder[MapTy] = decoderWithParser(MapTy.from)
  given Encoder[MapTy] = encoderWithStringifier(stringify)
  given Decoder[RecordTy] = decoderWithParser(RecordTy.from)
  given Encoder[RecordTy] = encoderWithStringifier(stringify)
  given Decoder[ListTy] = decoderWithParser(ListTy.from)
  given Encoder[ListTy] = encoderWithStringifier(stringify)
  given Decoder[ValueTy] = decoderWithParser(ValueTy.from)
  given Encoder[ValueTy] = encoderWithStringifier(stringify)
  given Decoder[AstTy] = decoderWithParser(AstTy.from)
  given Encoder[AstTy] = encoderWithStringifier(stringify)
  given Decoder[Ty] = decoderWithParser(Ty.from)
  given Encoder[Ty] = encoderWithStringifier(stringify)
  given [T](using decoder: Decoder[T]): Decoder[Map[String, T]] = strMapDecoder
  given [T](using encoder: Encoder[T]): Encoder[Map[String, T]] = strMapEncoder

  // type modeling
  given Decoder[TyModel] = decoderWithParser(TyModel.from)
  given Encoder[TyModel] = encoderWithStringifier(stringify)
  given Decoder[TyDecl] = decoderWithParser(TyDecl.from)
  given Encoder[TyDecl] = encoderWithStringifier(stringify)
  given Decoder[TyDecl.Elem] = decoderWithParser(TyDecl.Elem.from)
  given Encoder[TyDecl.Elem] = encoderWithStringifier(stringify)
  given Decoder[FieldMap] = decoderWithParser(FieldMap.from)
  given Encoder[FieldMap] = encoderWithStringifier(stringify)
  given fmeDec: Decoder[Binding] = decoderWithParser(Binding.from)
  given fmeEnc: Encoder[Binding] = encoderWithStringifier(stringify)

  // TODO: type errors

  // TODO: type error points
  given typeErrorPointEncoder: Encoder[TypeErrorPoint] =
    Encoder.instance {
      case a @ ArgAssignPoint(CallPoint(caller, callsite, callee), idx) =>
        Json.obj(
          "target" -> callsite.id.asJson,
          "caller" -> caller.name.asJson,
          "callee" -> callee.name.asJson,
          "param" -> a.param.lhs.name.asJson,
          "location" -> stringify(callsite.callInst.langOpt).trim().asJson
        )
      case InternalReturnPoint(func, node, irReturn) =>
        Json.obj(
          "target" -> node.id.asJson,
          "caller" -> func.name.asJson,
          "callee" -> "".asJson,
          "param" -> "".asJson,
          "location" -> stringify(irReturn.langOpt).trim().asJson
        )
      case _ => Json.obj("Todo" -> "Todo".asJson)
    }

  given Encoder[TypeError] =
    Encoder.instance {
      case error @ ParamTypeMismatch(point, argTy) =>
        Json.obj(
          "type" -> error.getClass.getSimpleName.asJson,
        ).deepMerge(typeErrorPointEncoder(point))
      case error @ ReturnTypeMismatch(point, retTy) =>
        Json.obj(
          "type" -> error.getClass.getSimpleName.asJson,
        ).deepMerge(typeErrorPointEncoder(point))
      case _ => Json.obj("Todo" -> "Todo".asJson)
    }
}
