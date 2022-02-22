package esmeta.ir.util

import esmeta.ir.*
import esmeta.spec.TypeModel
import esmeta.util.SystemUtils.*

/** extensions for programs */
extension (program: Program) {
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
