package esmeta.compiler

import esmeta.LINE_SEP
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMetaTest, RESULT_DIR}

/** compiler validity test */
class ValiditySmallTest extends CompilerTest {
  val name: String = "compilerValidityTest"

  // registration
  def init: Unit = {
    lazy val cur = ESMetaTest.program.completeFuncs.map(_.name).toSet
    check("compilation") { cur }
    val path = s"$RESULT_DIR/complete-funcs"
    val prev = optional(readFile(path).split(LINE_SEP).toSet).getOrElse(cur)
    check("complete IR functions") { assert(prev subsetOf cur) }
    if (prev != cur) dumpFile(cur.toList.sorted.mkString(LINE_SEP), path)
  }

  init
}
