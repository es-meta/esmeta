package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.{Expr, Func, Inst, Program, Ref}
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

class PartialEvaluator(
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting function */
  lazy val result: Func = targetFunc

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
    case IExpr(expr)                   => inst.toList
    case ILet(lhs, expr)               => inst.toList
    case IAssign(ref, expr)            => inst.toList
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
  ): Program =
    Program(
      funcs = prog.funcs.map(
        new PartialEvaluator(prog, _, log, output, logPW).result,
      ),
      spec = prog.spec,
    )
}
