package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.{Expr, Func, Inst, Program, Ref}
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

class PartialEvaluator(
  val pst: PState,
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting function */
  lazy val result: Func = targetFunc

  def peval(ref: Ref): Ref = ref match
    case Field(base, expr) => Field(peval(base), peval(expr))
    case _                 => ref

  def peval(expr: Expr): Expr = expr match
    case EParse(code, rule)                       => expr
    case EGrammarSymbol(name, params)             => expr
    case ESourceText(expr)                        => expr
    case EYet(msg)                                => expr
    case EContains(list, expr)                    => expr
    case ESubstring(expr, from, to)               => expr
    case ETrim(expr, isStarting)                  => expr
    case ERef(ref)                                => expr
    case EUnary(uop, expr)                        => expr
    case EBinary(bop, left, right)                => expr
    case EVariadic(vop, exprs)                    => expr
    case EMathOp(mop, args)                       => expr
    case EConvert(cop, expr)                      => expr
    case EExists(ref)                             => expr
    case ETypeOf(base)                            => expr
    case EInstanceOf(base, target)                => expr
    case ETypeCheck(base, ty)                     => expr
    case ESizeOf(base)                            => expr
    case EClo(fname, captured)                    => expr
    case ECont(fname)                             => expr
    case EDebug(expr)                             => expr
    case ERandom()                                => expr
    case ESyntactic(name, args, rhsIdx, children) => expr
    case ELexical(name, expr)                     => expr
    case ERecord(tname, pairs)                    => expr
    case EMap(pairs)                              => expr
    case EList(exprs)                             => expr
    case ECopy(obj)                               => expr
    case EKeys(map, intSorted)                    => expr
    case EMath(n)                                 => expr
    case EInfinity(pos)                           => expr
    case ENumber(double)                          => expr
    case EBigInt(bigInt)                          => expr
    case EStr(str)                                => expr
    case EBool(b)                                 => expr
    case EUndef()                                 => expr
    case ENull()                                  => expr
    case EEnum(name)                              => expr
    case ECodeUnit(c)                             => expr

  def peval(inst: Inst): List[Inst] = inst match
    case IExpr(expr) => inst.toList

    case ILet(lhs, expr) =>
      val pexpr = peval(expr)
      pst.define(lhs, pexpr)
      List(ILet(lhs, pexpr))

    case IAssign(ref, expr) =>
      val pref = peval(ref)
      val pexpr = peval(expr)
      pref.knownTarget match
        case Some(tgt) =>
          pst.update(tgt, pexpr)
        case None =>
          // kill over approximation of ref.
          ???
      List(IAssign(pref, pexpr))

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

  extension (ref: Ref)
    def knownTarget: Option[RefTarget] = ref match
      case x: Var => Some(VarTarget(x))
      case Field(base, expr) =>
        for {
          b <- base.knownTarget
          e <- expr.knownValue
        } yield FieldTarget(pst(b), e)

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/log"))
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
