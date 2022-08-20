package esmeta.extractor

import esmeta.spec.Summary
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMetaTest, RESULT_DIR}

/** extractor validity test */
class ValiditySmallTest extends ExtractorTest {
  val name: String = "extractorValidityTest"

  // registration
  def init: Unit = {
    val path = s"$RESULT_DIR/spec-summary"
    val cur = ESMetaTest.spec.summary
    val prev = optional(Summary.fromFile(path)).getOrElse(cur)
    check("git version") { assert(prev.version == cur.version) }
    check("grammar") { assert(prev.grammar == cur.grammar) }
    check("algorithms") { assert(prev.algos.complete <= cur.algos.complete) }
    check("steps") { assert(prev.steps.complete <= cur.steps.complete) }
    check("tables") { assert(prev.tables == cur.tables) }
    check("type model") { assert(prev.typeModel == cur.typeModel) }
    dumpFile(cur, path)
  }

  init
}
