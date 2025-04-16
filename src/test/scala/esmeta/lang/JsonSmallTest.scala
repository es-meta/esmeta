package esmeta.lang

import esmeta.{error as _, *}
import esmeta.lang.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*
import io.circe.parser.*;

import scala.collection.mutable.{Set as MSet};

/** JSON test */
class JsonSmallTest extends LangTest {
  val name: String = "langJsonTest"

  def init: Unit = {

    val langTypes = (
      for {
        func <- ESMetaTest.program.funcs
        algo <- func.algo
      } yield for {
        langTy <- collect(algo.head)
      } yield langTy
    ).flatten.distinct

    langTypes.foreach { langType =>
      checkEqual(
        s"lang.Type(ty : ty.Ty = ${langType.ty.toString}) ⊑ langTypeAfterDecoding",
      )(
        decode[Type](langType.asJson.noSpaces)
          .map(langType.ty.toValue <= _.ty.toValue)
          .getOrElse(false) -> true,
      )
    }
  }

  private def collect(h: spec.Head): List[lang.Type] = {
    val types = MSet.empty[lang.Type]
    val _ = {
      h match
        case spec.AbstractOperationHead(isHostDefined, name, params, retTy) =>
          for {
            param <- params
          } types += param.ty
          types += retTy
        case spec.NumericMethodHead(baseTy, name, params, retTy) =>
          for {
            param <- params
          } types += param.ty
          types += retTy
        case spec.SyntaxDirectedOperationHead(
              target,
              methodName,
              isStatic,
              withParams,
              retTy,
            ) =>
          for {
            param <- withParams
          } types += param.ty
          types += retTy
        case spec.ConcreteMethodHead(concMethodName, receiver, params, retTy) =>
          for {
            param <- params
          } types += param.ty
          types += retTy
        case spec.InternalMethodHead(methodName, receiver, params, retTy) =>
          for {
            param <- params
          } types += param.ty
          types += retTy
        case spec.BuiltinHead(path, params, retTy) =>
          for {
            param <- params
          } types += param.ty
          types += retTy
    };
    types.toList
  }

  init
}
