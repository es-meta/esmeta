package esmeta.peval.util.walker

import esmeta.cfg.{CFG}
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
  * @param cfg
  */
class RenameWalker private (renamer: Renamer, pst: PState, cfg: CFG)
  extends Walker {

  private val defSet = MSet.empty[Local]

  def defs = Set.from[Local](defSet)

  given CFG = cfg
  override def walk(x: Name): Name = renamer.get(x, pst.context)
  override def walk(x: Temp): Temp = renamer.get(x, pst.context)
}

object RenameWalker {
  def apply(inst: Inst)(using renamer: Renamer, pst: PState, cfg: CFG) =
    new RenameWalker(renamer, pst, cfg).walk(inst)

  def apply(expr: Expr)(using renamer: Renamer, pst: PState, cfg: CFG) =
    new RenameWalker(renamer, pst, cfg).walk(expr)
}
