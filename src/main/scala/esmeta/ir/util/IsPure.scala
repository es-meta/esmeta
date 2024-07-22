package esmeta.ir.util

import esmeta.ir.*

/** Checks if an Expr is pure, if true then eval(e) does NOT changes state */
def isPure(e: Expr): Boolean = e match
  case EComp(tyExpr, valExpr, tgtExpr) =>
    isPure(tyExpr) && isPure(valExpr) && isPure(tgtExpr)
  case EIsCompletion(expr)          => isPure(expr)
  case EReturnIfAbrupt(expr, check) => isPure(expr)
  case EPop(list, front)            => false
  case EParse(code, rule)           => false // notSure
  case ENt(name, params)            => true
  case ESourceText(expr)            => isPure(expr)
  case EYet(msg)                    => false
  case EContains(list, expr)        => isPure(list) && isPure(expr)
  case ESubstring(expr, from, to) =>
    isPure(expr) && isPure(from) && to.map(isPure).getOrElse(true)
  case ETrim(expr, leading, trailing) => isPure(expr)
  case ERef(ref)                      => isPure(ref)
  case EUnary(uop, expr)              => isPure(expr)
  case EBinary(bop, left, right)      => isPure(left) && isPure(right)
  case EVariadic(vop, exprs)          => exprs.forall(isPure)
  case EClamp(target, lower, upper) =>
    isPure(target) && isPure(lower) && isPure(upper)
  case EMathOp(mop, args)       => args.forall(isPure)
  case EConvert(cop, expr)      => isPure(expr)
  case ETypeOf(base)            => isPure(base)
  case ETypeCheck(base, tyExpr) => isPure(base) && isPure(tyExpr)
  case EDuplicated(list)        => isPure(list)
  case EIsArrayIndex(expr)      => isPure(expr)
  case EClo(fname, captured)    => true
  case ECont(fname)             => true
  case EDebug(expr)             => isPure(expr)
  case ERandom()                => false
  case ESyntactic(name, args, rhsIdx, children) =>
    children.forall((e) => e.map(isPure).getOrElse(true))
  case ELexical(name, expr)  => isPure(expr)
  case EMap(tname, props)    => false
  case EList(exprs)          => false
  case EListConcat(exprs)    => false
  case ESymbol(desc)         => false
  case ECopy(obj)            => false
  case EKeys(map, intSorted) => false
  case EGetChildren(ast)     => false
  case EGetItems(nt, ast)    => isPure(nt) // && isPure(ast)
  case EMath(n)              => true
  case EInfinity(pos)        => true
  case ENumber(double)       => true
  case EBigInt(bigInt)       => true
  case EStr(str)             => true
  case EBool(b)              => true
  case EUndef()              => true
  case ENull()               => true
  case EAbsent()             => true
  case EEnum(name)           => true
  case ECodeUnit(c)          => true

def isPure(r: Ref): Boolean = r match
  case Prop(ref, expr) => isPure(r) && isPure(expr)
  case Global(name)    => true
  case Name(name)      => true
  case Temp(idx)       => true
