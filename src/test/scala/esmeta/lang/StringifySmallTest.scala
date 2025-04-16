package esmeta.lang

import esmeta.ESMetaTest
import esmeta.lang.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

import scala.collection.mutable.{Set as MSet};

/** JSON test */
class StringifySmallTest extends LangTest {
  val name: String = "langStringifyTest"

  def init: Unit = {
    import LangTest.*

    val langTypes = (
      for {
        func <- ESMetaTest.program.funcs
        algo <- func.algo
      } yield for {
        langTy <- LangTest.collectTypes(algo.head)
      } yield langTy
    ).flatten.distinct

    langTypes.foreach { langType =>
      checkEqual(
        s"lang.Type(ty : ty.Ty = ${langType.ty.toString}).toString.trim().nonEmpty",
      )(
        (langType.toString.trim().nonEmpty) -> true,
      )
    }
  }

  init
}
