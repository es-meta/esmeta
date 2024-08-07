package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.{Expr, Func, Inst, Program, Ref}
import esmeta.ir.*
import esmeta.peval.domain.*
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import esmeta.ty.InfinityT
import esmeta.interpreter.{Interpreter}

class PartialEvaluator(
  val pst: PState,
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting function */
  lazy val result: Func = peval(targetFunc)

  def peval(ref: Ref): PRefTarget = ref match
    case Field(base, expr) => ???
    // (peval(base), peval(expr)) match
    // case (r : PartialRef.R, e : PartialValue.V ) => PartialRef.R(Field(r, e), )
    // case _ => ???
    //  Field(peval(base), peval(expr))
    case v: Var => PRefTarget.RT(VarTarget(v), ref)

  def peval(expr: Expr): PValue =
    if (log) then
      pw.print(s"[peval][Expr] $expr")
      pw.flush()
    val e = expr
    expr match
      case EParse(code, rule)           => PValue.E(e, e)
      case EGrammarSymbol(name, params) => PValue.E(e, e)
      case ESourceText(expr)            => PValue.E(e, e)
      case EYet(msg)                    => PValue.E(e, e)
      case EContains(list, expr)        => PValue.E(e, e)
      case ESubstring(expr, from, to)   => PValue.E(e, e)
      case ETrim(expr, isStarting)      => PValue.E(e, e)
      case ERef(ref) =>
        val pref = peval(ref)
        pref match
          case PRefTarget.R(absref, ref)  => ???
          case PRefTarget.RT(target, ref) => ???

      case EUnary(uop, expr) =>
        val pexpr = peval(expr)
        pexpr match
          case e: PValue.E =>
            PValue.E(
              e.absExpr,
              EUnary(uop, pexpr.asValidExpr),
            )
          case v: PValue.V =>
            PValue.V(
              Interpreter.eval(uop, v.value),
              EUnary(uop, pexpr.asValidExpr),
            )

      case EBinary(bop, left, right) =>
        val pleft = peval(left)
        val pright = peval(right)
        (pleft, pright) match
          case (v1: PValue.V, v2: PValue.V) =>
            PValue.V(
              Interpreter.eval(bop, v1.value, v2.value),
              EBinary(bop, pleft.asValidExpr, pright.asValidExpr),
            )
          case (p1, p2) =>
            PValue.E(
              // TODO : use
              e,
              EBinary(bop, pleft.asValidExpr, pright.asValidExpr),
            )

      case EVariadic(vop, exprs)                    => PValue.E(e, e)
      case EMathOp(mop, args)                       => PValue.E(e, e)
      case EConvert(cop, expr)                      => PValue.E(e, e)
      case EExists(ref)                             => PValue.E(e, e)
      case ETypeOf(base)                            => PValue.E(e, e)
      case EInstanceOf(base, target)                => PValue.E(e, e)
      case ETypeCheck(base, ty)                     => PValue.E(e, e)
      case ESizeOf(base)                            => PValue.E(e, e)
      case EClo(fname, captured)                    => PValue.E(e, e)
      case ECont(fname)                             => PValue.E(e, e)
      case EDebug(expr)                             => PValue.E(e, e)
      case ERandom()                                => PValue.E(e, e)
      case ESyntactic(name, args, rhsIdx, children) => PValue.E(e, e)
      case ELexical(name, expr)                     => PValue.E(e, e)
      case ERecord(tname, pairs)                    => PValue.E(e, e)
      case EMap(pairs)                              => PValue.E(e, e)
      case EList(exprs)                             => PValue.E(e, e)
      case ECopy(obj)                               => PValue.E(e, e)
      case EKeys(map, intSorted)                    => PValue.E(e, e)

      case EMath(n)     => PValue.V(Math(n), e)
      case EInfinity(p) => PValue.V(Infinity(p), e)
      case ENumber(d)   => PValue.V(Number(d), e)
      case EBigInt(n)   => PValue.V(BigInt(n), e)
      case EStr(str)    => PValue.V(Str(str), e)
      case EBool(b)     => PValue.V(Bool(b), e)
      case EUndef()     => PValue.V(Undef, e)
      case ENull()      => PValue.V(Null, e)
      case EEnum(name)  => PValue.V(Enum(name), e)
      case ECodeUnit(c) => PValue.V(CodeUnit(c), e)

  def peval(inst: Inst): List[Inst] =
    if (log) then
      pw.print(s"[peval][Inst] $inst")
      pw.close()
    inst match
      case IExpr(expr) => List(IExpr(peval(expr).asValidExpr))

      case ILet(lhs, expr) =>
        val pexpr = peval(expr)
        pst.define(lhs, pexpr)
        List(ILet(lhs, pexpr.asValidExpr))

      case IAssign(ref, expr) =>
        val pref = peval(ref)
        val pexpr = peval(expr)
        pref match
          case PRefTarget.R(ar, r) => ???
          case PRefTarget.RT(tgt, r) =>
            pst.update(tgt, pexpr)
            List(IAssign(pref.asValidRef, pexpr.asValidExpr))

      case IExpand(base, expr)           => inst.toList
      case IDelete(base, expr)           => inst.toList
      case IPush(elem, list, front)      => inst.toList
      case IPop(lhs, list, front)        => inst.toList
      case IReturn(expr)                 => inst.toList
      case IAssert(expr)                 => inst.toList
      case IPrint(expr)                  => inst.toList
      case INop()                        => Nil
      case IIf(cond, thenInst, elseInst) => inst.toList
      case IWhile(cond, body)            => inst.toList
      case ICall(lhs, fexpr, args)       => inst.toList
      case ISdoCall(lhs, base, op, args) => inst.toList
      case ISeq(insts)                   => ISeq(insts.flatMap(peval)).toList

  def peval(func: Func): Func = Func(
    func.main,
    func.kind,
    func.name,
    func.params,
    func.retTy,
    peval(func.body).toInst,
    func.algo,
  )

  extension (insts: Iterable[Inst])
    def toInst: Inst =
      insts.size match
        case 0 => ISeq(Nil)
        case 1 => insts.head
        case _ => ISeq(insts.toList)

  extension (expr: Expr)
    def knownValue: Option[Value] = expr match
      case lit: LiteralExpr =>
        Some(
          lit match
            case EMath(n)        => Math(n)
            case EInfinity(pos)  => Infinity(pos)
            case ENumber(double) => Number(double)
            case EBigInt(bigInt) => BigInt(bigInt)
            case EStr(str)       => Str(str)
            case EBool(b)        => Bool(b)
            case EUndef()        => Undef
            case ENull()         => Null
            case EEnum(name)     => Enum(name)
            case ECodeUnit(c)    => CodeUnit(c),
        )
      case _ => None

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/func/${targetFunc.name}"))
}

object PartialEvaluator {
  def apply(
    prog: Program,
    log: Boolean = false,
    output: Option[String] = None,
    logPW: Option[PrintWriter] = None,
    simplify: Boolean = false,
  ): Program =
    val newProg = Program(
      funcs = prog.funcs.map(func =>
        new PartialEvaluator(
          PState(func),
          prog,
          func,
          log,
          output,
          logPW,
        ).result,
      ),
      spec = prog.spec,
    )
    if (simplify) then Simplifier(newProg) else newProg
}
