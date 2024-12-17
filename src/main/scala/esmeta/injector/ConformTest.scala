package esmeta.injector

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.NoGraalError
import java.util.concurrent.TimeoutException
import esmeta.es.*
import esmeta.es.util.*
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.util.*

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  exitTag: ExitTag,
  async: Boolean,
  assertions: Vector[Assertion],
) extends InjectorElem
  with UId

object ConformTest {

  /** Create a test using init state and exit state */
  def createTest(cfg: CFG, exitSt: State): ConformTest =
    new Injector(cfg, exitSt, false).conformTest
}
