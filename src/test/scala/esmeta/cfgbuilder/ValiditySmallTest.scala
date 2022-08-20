package esmeta.cfgbuilder

import esmeta.ESMetaTest

/** compiler validity test */
class ValiditySmallTest extends CFGBuilderTest {
  val name: String = "cfgBuilderValidityTest"

  // registration
  def init: Unit = {
    lazy val cfg = ESMetaTest.cfg
    check("CFG build") { cfg }
  }

  init
}
