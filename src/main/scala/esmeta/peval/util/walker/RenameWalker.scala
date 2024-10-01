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
class RenameWalker(renamer: Renamer, pst: PState, cfg: CFG) extends Walker {

  private val defSet = MSet.empty[Local]

  def defs = Set.from[Local](defSet)

  given CFG = cfg
  override def walk(x: Name): Name = renamer.get(x, pst.context)
  override def walk(x: Temp): Temp = renamer.get(x, pst.context)

  override def walk(expr: Expr): Expr = (expr match
    case EParse(code, rule) =>
      EParse(walk(code), walk(rule))
    case EGrammarSymbol(name, params) =>
      EGrammarSymbol(walk(name), walkList(params, walk))
    case ESourceText(expr) =>
      ESourceText(walk(expr))
    case EYet(msg) =>
      EYet(walk(msg))
    case EContains(list, elem) =>
      EContains(walk(list), walk(elem))
    case ESubstring(expr, from, to) =>
      ESubstring(walk(expr), walk(from), walkOpt(to, walk))
    case ETrim(expr, isStarting) =>
      ETrim(walk(expr), walk(isStarting))
    case ERef(ref) => /* changed */
      val newRef = walk(ref)
      newRef match
        case l: Local =>
          // this is just renamer - so .. Try
          Try(pst(l)).toOption match
            case Some(Known(v)) if v.isLiteralValue => v.toExpr
            case _                                  => ERef(newRef)
        case _ => ERef(newRef)
    case EUnary(uop, expr) =>
      EUnary(walk(uop), walk(expr))
    case EBinary(bop, left, right) =>
      EBinary(walk(bop), walk(left), walk(right))
    case EVariadic(vop, exprs) =>
      EVariadic(walk(vop), walkList(exprs, walk))
    case EMathOp(mop, exprs) =>
      EMathOp(walk(mop), walkList(exprs, walk))
    case EConvert(cop, expr) =>
      EConvert(walk(cop), walk(expr))
    case EExists(ref) =>
      EExists(walk(ref))
    case ETypeOf(base) =>
      ETypeOf(walk(base))
    case EInstanceOf(expr, target) =>
      EInstanceOf(walk(expr), walk(target))
    case ETypeCheck(expr, ty) =>
      ETypeCheck(walk(expr), walk(ty))
    case ESizeOf(expr) =>
      ESizeOf(walk(expr))
    case EClo(fname, captured) =>
      EClo(walk(fname), walkList(captured, walk))
    case ECont(fname) =>
      ECont(walk(fname))
    case EDebug(expr) =>
      EDebug(walk(expr))
    case expr: ERandom     => walk(expr)
    case expr: AstExpr     => walk(expr)
    case expr: AllocExpr   => walk(expr)
    case expr: LiteralExpr => walk(expr)
  ).setLangOpt(expr.langOpt)

// random number expressions
  override def walk(rand: ERandom): ERandom = ERandom()

// abstract syntax tree (AST) expressions
  override def walk(ast: AstExpr): AstExpr = ast match
    case ESyntactic(name, args, rhsIdx, children) =>
      ESyntactic(
        walk(name),
        walkList(args, walk),
        walk(rhsIdx),
        walkList(children, walkOpt(_, walk))
      )
    case ELexical(name, expr) =>
      ELexical(walk(name), walk(expr))

// allocation expressions
  override def walk(alloc: AllocExpr): AllocExpr = alloc match
    case ERecord(tname, fields) =>
      ERecord(
        walk(tname),
        walkList(fields, { case (p, e) => (walk(p), walk(e)) })
      )
    case EMap(ty, pairs) =>
      EMap(walkPair(ty, walk, walk), walkList(pairs, walkPair(_, walk, walk)))
    case EList(exprs) =>
      EList(walkList(exprs, walk))
    case ECopy(obj) =>
      ECopy(walk(obj))
    case EKeys(map, intSorted) =>
      EKeys(walk(map), walk(intSorted))

// literals
  override def walk(lit: LiteralExpr): LiteralExpr = lit
}
