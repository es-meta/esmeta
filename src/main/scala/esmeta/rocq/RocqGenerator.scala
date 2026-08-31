package esmeta.rocq

import esmeta.ir.{Func, Program}
import esmeta.util.SystemUtils.dumpFile

/** Generates one self-contained shallow Rocq module for an IR program. */
class RocqGenerator(
  program: Program,
  stringifier: RocqStringifier,
) {
  def this(program: Program) =
    this(program, new RocqStringifier(program.funcs))

  def this(program: Program, proofObligations: Boolean) =
    this(program, new RocqStringifier(program.funcs, proofObligations))

  def apply(func: Func): String = stringifier(func)

  def translate(func: Func): String = stringifier.translate(func)

  def source: String = stringifier.program(program.funcs)

  def dumpTo(filename: String): Unit =
    dumpFile(
      name = "Rocq ITree module",
      data = source,
      filename = filename,
    )
}

object RocqGenerator {
  def apply(program: Program): RocqGenerator = new RocqGenerator(program)

  def apply(
    program: Program,
    proofObligations: Boolean,
  ): RocqGenerator = new RocqGenerator(program, proofObligations)
}
