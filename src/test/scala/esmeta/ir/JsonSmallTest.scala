package esmeta.ir

import esmeta.ir.util.JsonProtocol.given
import io.circe.*, io.circe.syntax.*, io.circe.generic.auto.*
import esmeta.ESMetaTest

/** JSON test */
class JsonSmallTest extends IRTest {
  val name: String = "irJsonTest"

  // registration
  def init: Unit = {
    import IRTest.*

    ESMetaTest.program.funcs

  }

  init
}
