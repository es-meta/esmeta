package esmeta

import esmeta.BASE_DIR
import esmeta.util.GenCompl
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** test for .completion file */
class ComplTinyTest extends ESMetaTest {
  def category: String = "general"
  val name: String = "complTest"

  // registration
  def init: Unit = {
    val expected = GenCompl.content
    val result = optional(readFile(s"$BASE_DIR/.completion")).getOrElse("")
    check("completion") { assert(result == expected) }
  }
  init
}
