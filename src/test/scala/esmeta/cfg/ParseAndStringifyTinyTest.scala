package esmeta.cfg

import esmeta.util.BaseUtils.*
import esmeta.cfg.*

class ParseAndStringifyTinyTest extends CFGTest {
  val name: String = "cfgParseAndStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // control flow graphs (CFGs)
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("CFG", CFG)( /* TODO */ )

    // -------------------------------------------------------------------------
    // functions
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Func", Func)( /* TODO */ )

    // -------------------------------------------------------------------------
    // parameters
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Param", Param)( /* TODO */ )

    // -------------------------------------------------------------------------
    // nodes
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Node", Node)( /* TODO */ )

    // -------------------------------------------------------------------------
    // instructions
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Inst", Inst)( /* TODO */ )

    // -------------------------------------------------------------------------
    // expressions
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Expr", Expr)( /* TODO */ )

    // -------------------------------------------------------------------------
    // operators
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("UOp", UOp)( /* TODO */ )
    checkParseAndStringify("BOp", BOp)( /* TODO */ )
    checkParseAndStringify("COp", COp)( /* TODO */ )

    // -------------------------------------------------------------------------
    // references
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Ref", Ref)( /* TODO */ )

    // -------------------------------------------------------------------------
    // types
    // -------------------------------------------------------------------------
    // tests
    checkParseAndStringify("Type", Type)( /* TODO */ )
  }

  init
}
