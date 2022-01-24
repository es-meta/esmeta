package esmeta.cfg

import esmeta.ESMetaTest

trait CFGTest extends ESMetaTest {
  def category: String = "cfg"

  // tests for CFG parser
  def cfgParseTest(cfg: CFG): CFG = {
    val newCFG = CFG.from(cfg.toString)
    assert(cfg == newCFG)
    cfg
  }
  def cfgParseTest(str: String): CFG = cfgParseTest(CFG.from(str))
  def cfgParseTestFile(filename: String): CFG =
    cfgParseTest(CFG.fromFile(filename))
}
