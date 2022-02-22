package esmeta.ir.util

import esmeta.cfg.CFG
import esmeta.cfg.util.Builder
import esmeta.ir.*
import esmeta.spec.TypeModel
import esmeta.util.SystemUtils.*

/** extensions for programs */
extension (program: Program) {

  /** convert to a control-flow graph (CFG) */
  def toCFG: CFG = Builder(program)

  /** get a type model */
  def typeModel: TypeModel = program.spec.typeModel

  /** dump program of specs */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    for {
      func <- program.funcs
      name = func.name
      filename = s"$baseDir/${name.replace("/", "")}.ir"
    } dumpFile(func, filename)
}
