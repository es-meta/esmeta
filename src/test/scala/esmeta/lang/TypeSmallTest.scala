package esmeta.lang

import esmeta.LINE_SEP
import esmeta.ESMetaTest
import esmeta.lang.*
import esmeta.lang.util.JsonProtocol.given
import esmeta.spec
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*
import io.circe.parser.*;

import scala.collection.mutable.{Set as MSet};

// TODO refactor after supporting precise stringifier for lang types
/** test for metalanguage type in spec */
class TypeSmallTest extends LangTest {
  val name: String = "langTypeTest"

  // TODO refactor after supporting walkers for lang
  def collectTypes(h: spec.Head): List[Type] = {
    val types = MSet.empty[Type]
    val _ = {
      h match
        case spec.AbstractOperationHead(isHostDefined, name, params, retTy) =>
          for { param <- params } types += param.ty
          types += retTy
        case spec.NumericMethodHead(baseTy, name, params, retTy) =>
          for { param <- params } types += param.ty
          types += baseTy
          types += retTy
        case spec.SyntaxDirectedOperationHead(target, _, _, withParams, rty) =>
          for {
            param <- withParams
          } types += param.ty
          for {
            param <- target.toList
            rhsParam <- param.rhsParams
          } types += rhsParam.ty
          types += rty
        case spec.ConcreteMethodHead(concMethodName, receiver, params, retTy) =>
          for { param <- params } types += param.ty
          types += retTy
        case spec.InternalMethodHead(methodName, receiver, params, retTy) =>
          for { param <- params } types += param.ty
          types += receiver.ty
          types += retTy
        case spec.BuiltinHead(path, params, retTy) =>
          for { param <- params } types += param.ty
          types += retTy
    };
    types.toList
  }

  def init: Unit = {
    import LangTest.*

    val langTypes = (
      for {
        func <- ESMetaTest.program.funcs
        algo <- func.algo
      } yield for {
        langTy <- collectTypes(algo.head)
      } yield langTy
    ).flatten.distinct

    check("Stringify") {
      val failedTypes = for {
        langType <- langTypes
        pass = langType.toString.trim.nonEmpty
        if !pass
      } yield langType
      if (failedTypes.nonEmpty) {
        error(
          s"failed to stringify ${failedTypes.size} types:" +
          failedTypes.map(LINE_SEP + "* " + _.ty.toString).mkString,
        )
      }
    }
  }

  init
}
