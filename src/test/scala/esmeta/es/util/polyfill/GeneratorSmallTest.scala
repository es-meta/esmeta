package esmeta.es.util.polyfill

import esmeta.*
import esmeta.es.util.polyfill.dsl.*
import esmeta.lang.*

/** tests for the polyfill generator, with and without the rewrite rules */
class GeneratorSmallTest extends PolyfillTest {
  val name: String = "polyfillGeneratorTest"

  // registration
  def init: Unit = {
    import ESMetaTest.*

    // generation fails on any unsupported step, so this also checks that every
    // target is supported
    lazy val basic = Generator(spec, None).map(p => p.name -> p.toString).toMap
    lazy val opt = Generator(spec, Some(POLYFILL_RULES_DIR))
      .map(p => p.name -> p.toString)
      .toMap
    def code(polyfills: Map[String, String], name: String): String =
      polyfills.getOrElse(name, fail(s"no polyfill for $name"))

    // -------------------------------------------------------------------------
    // DSL parsing
    // -------------------------------------------------------------------------
    check("operation names") {
      // `_` is allowed only in the DSL, for internal operations
      val dsl = DSLParser().parseExpr("IN__SetDataHas(_s_, _x_)")
      assert(
        dsl == InvokeAbstractOperationExpression(
          "IN__SetDataHas",
          List(refExpr("s"), refExpr("x")),
        ),
      )
      val lang = scala.util.Try(Expression.from("IN__SetDataHas(_s_, _x_)"))
      assert(!lang.toOption.contains(dsl))
    }

    check("rules") {
      val rules = DSLRuleParser.parseDir(POLYFILL_RULES_DIR)
      assert(rules.nonEmpty)
      assert(rules.map(_.name).distinct.length == rules.length)
    }

    // -------------------------------------------------------------------------
    // generation
    // -------------------------------------------------------------------------
    check("same targets") {
      assert(basic.keySet == opt.keySet)
    }

    check("no meta-variables left") {
      for ((name, c) <- opt) assert(!c.contains("{{"), name)
    }

    check("basic keeps spec lists") {
      // the rewrite rules are not applied without `-gen-poly:opt`
      for ((name, c) <- basic) assert(!c.contains("IN__SetData"), name)
      for ((name, c) <- basic) assert(!c.contains("IN__MapData"), name)
    }

    check("opt rewrites SetData") {
      val c = code(opt, "INTRINSICS.Set.prototype.has")
      assert(c.contains("IN__SetDataHas"))
    }

    check("opt drops SetDataIndex") {
      // every use of the index is rewritten, so the lookup itself is dropped
      for (name <- List("difference", "symmetricDifference")) {
        val c = code(opt, s"INTRINSICS.Set.prototype.$name")
        assert(!c.contains("SetDataIndex"), name)
        assert(c.contains("IN__SetDataHas"), name)
      }
    }
  }

  def refExpr(x: String): Expression = ReferenceExpression(Variable(x))

  init
}
