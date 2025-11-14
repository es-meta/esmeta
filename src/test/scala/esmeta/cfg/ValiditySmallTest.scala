package esmeta.cfg

import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMetaTest, RESULT_DIR}

/** cfg validity test */
class ValiditySmallTest extends CFGTest {
  val name: String = "cfgValidityTest"

  // registration
  def init: Unit = {
    check("fingerprint") {
      val path = s"$RESULT_DIR/cfg-fingerprint"
      val prev = optional(readFile(path).trim).getOrElse("<none>")
      val cur = ESMetaTest.cfg.fingerprint
      if (prev != cur) {
        fail(
          "function/node IDs have changed -- CFG fingerprint mismatch:" +
          s"\n* previous: $prev" +
          s"\n* current : $cur",
        )
        dumpFile(cur, path)
      }
    }
  }

  init
}
