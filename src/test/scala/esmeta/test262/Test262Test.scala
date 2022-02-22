package esmeta.test262

import esmeta.*
import esmeta.spec.Spec
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

trait Test262Test extends ESMetaTest {
  def category: String = "test262"

  // predefined data
  lazy val logDir = s"$LOG_DIR/test262_$dateStr"
  def spec = ESMetaTest.spec
}
object Test262Test {
  lazy val test262 = Test262(ESMetaTest.spec)
}
