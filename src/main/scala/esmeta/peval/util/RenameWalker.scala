package esmeta.peval.util

import esmeta.ir.*
import esmeta.ir.util.Walker
import esmeta.peval.*
import esmeta.peval.pstate.*
import esmeta.peval.util.*

import scala.collection.mutable.{Set as MSet}
import scala.util.{Try}

/** Note: RenameWalker should never "call" - only works in current context
  *
  * @param renamer
  * @param ctx
  */
class RenameWalker private (renamer: Renamer, pst: PState) extends Walker {

  private val defSet = MSet.empty[Local]

  def defs = Set.from[Local](defSet)
  override def walk(x: Name): Name = renamer.get(x, pst.context)
  override def walk(x: Temp): Temp = renamer.get(x, pst.context)
}

object RenameWalker {
  def apply(inst: Inst)(using renamer: Renamer, pst: PState) =
    new RenameWalker(renamer, pst).walk(inst)

  def apply(expr: Expr)(using renamer: Renamer, pst: PState) =
    new RenameWalker(renamer, pst).walk(expr)
}
