package esmeta.ir

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }

/** IR Instructions */
type CondInst = Inst.IIf | Inst.IWhile
type CallInst = Inst.IApp | Inst.IAccess
type ArrowInst = Inst.IClo | Inst.ICont | Inst.IWithCont
type NormalInst = 
  Inst.IExpr |
  Inst.ILet |
  Inst.IAssign |
  Inst.IDelete |
  Inst.IAppend |
  Inst.IPrepend |
  Inst.IReturn |
  Inst.IThrow |
  Inst.IAssert |
  Inst.IPrint
enum Inst:
  case IIf(cond: Expr, thenInst: Inst, elseInst: Inst) 
  case IWhile(cond: Expr, body: Inst) 
  case IApp(id: Id, fexpr: Expr, args: List[Expr]) 
  case IAccess(id: Id, bexpr: Expr, expr: Expr, args: List[Expr]) 
  case IExpr(expr: Expr) 
  case ILet(id: Id, expr: Expr) 
  case IAssign(ref: Ref, expr: Expr) 
  case IDelete(ref: Ref) 
  case IAppend(expr: Expr, list: Expr) 
  case IPrepend(expr: Expr, list: Expr) 
  case IReturn(expr: Expr) 
  case IThrow(name: String) 
  case IAssert(expr: Expr) 
  case IPrint(expr: Expr) 
  case IClo(id: Id, params: List[Id], captured: List[Id], body: Inst) 
  case ICont(id: Id, params: List[Id], body: Inst) 
  case IWithCont(id: Id, params: List[Id], body: Inst) 
  case ISeq(insts: List[Inst]) 

/** IR Expressions */
enum Expr:
  case ENum(n: Double)
  case EINum(n: Long) 
  case EBigINum(b: BigInt) 
  case EStr(str: String) 
  case EBool(b: Boolean) 
  case EUndef 
  case ENull 
  case EAbsent 
  case EConst(name: String) 
  case EComp(ty: Expr, value: Expr, target: Expr) 
  case EMap(ty: Ty, props: List[(Expr, Expr)]) 
  case EList(exprs: List[Expr]) 
  case ESymbol(desc: Expr) 
  case EPop(list: Expr, idx: Expr) 
  case ERef(ref: Ref) 
  case EUOp(uop: UOp, expr: Expr) 
  case EBOp(bop: BOp, left: Expr, right: Expr) 
  case ETypeOf(expr: Expr) 
  case EIsCompletion(expr: Expr) 
  case EIsInstanceOf(base: Expr, name: String) 
  case EGetElems(base: Expr, name: String) 
  case EGetSyntax(base: Expr) 
  case EParseSyntax(code: Expr, rule: Expr, parserParams: List[Boolean]) 
  case EConvert(source: Expr, target: COp, flags: List[Expr]) 
  case EContains(list: Expr, elem: Expr) 
  case EReturnIfAbrupt(expr: Expr, check: Boolean) 
  case ECopy(obj: Expr) 
  case EKeys(mobj: Expr, intSorted: Boolean) 
  case ENotSupported(msg: String) 

/** IR References */
enum Ref:
  case RefId(id: Id) 
  case RefProp(ref: Ref, expr: Expr) 

/** IR Identifiers */
case class Id(name: String)

/** IR Types */
case class Ty(name: String)

/** IR Unary Operators */
enum UOp:
  case ONeg, ONot, OBNot 

/** IR Binary Operators */
enum BOp:
  case OPlus
  case OSub
  case OMul
  case OPow
  case ODiv
  case OUMod
  case OMod
  case OLt
  case OEq
  case OEqual
  case OAnd
  case OOr
  case OXor
  case OBAnd
  case OBOr
  case OBXOr
  case OLShift
  case OSRShift
  case OURShift

/** IR Convert Operators */
enum COp:
  case CStrToNum
  case CStrToBigInt
  case CNumToStr
  case CNumToInt
  case CNumToBigInt
  case CBigIntToNum
