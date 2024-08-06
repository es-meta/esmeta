package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR}
import esmeta.ir.{Expr, Func, Inst, Program, Ref}
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter

class PartialEvaluator(
  val program: Program,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting program */
  lazy val result: Program = program

  def peval(prog: Program): Program =
    Program(funcs = prog.funcs.map(peval), spec = prog.spec)

  def peval(expr: Expr): Either[Expr, Value] = expr match
    case EParse(code, rule)                       => ???
    case EGrammarSymbol(name, params)             => ???
    case ESourceText(expr)                        => ???
    case EYet(msg)                                => ???
    case EContains(list, expr)                    => ???
    case ESubstring(expr, from, to)               => ???
    case ETrim(expr, isStarting)                  => ???
    case ERef(ref)                                => ???
    case EUnary(uop, expr)                        => ???
    case EBinary(bop, left, right)                => ???
    case EVariadic(vop, exprs)                    => ???
    case EMathOp(mop, args)                       => ???
    case EConvert(cop, expr)                      => ???
    case EExists(ref)                             => ???
    case ETypeOf(base)                            => ???
    case EInstanceOf(base, target)                => ???
    case ETypeCheck(base, ty)                     => ???
    case ESizeOf(base)                            => ???
    case EClo(fname, captured)                    => ???
    case ECont(fname)                             => ???
    case EDebug(expr)                             => ???
    case ERandom()                                => ???
    case ESyntactic(name, args, rhsIdx, children) => ???
    case ELexical(name, expr)                     => ???
    case ERecord(tname, pairs)                    => ???
    case EMap(pairs)                              => ???
    case EList(exprs)                             => ???
    case ECopy(obj)                               => ???
    case EKeys(map, intSorted)                    => ???
    case EMath(n)                                 => Right(Math(n))
    case EInfinity(pos)                           => Right(Infinity(pos))
    case ENumber(double)                          => Right(Number(double))
    case EBigInt(bigInt)                          => Right(BigInt(bigInt))
    case EStr(str)                                => Right(Str(str))
    case EBool(b)                                 => Right(Bool(b))
    case EUndef()                                 => Right(Undef)
    case ENull()                                  => Right(Null)
    case EEnum(name)                              => Right(Enum(name))
    case ECodeUnit(c)                             => Right(CodeUnit(c))

  def peval(inst: Inst): List[Inst] = inst match
    case IExpr(expr)                   => ???
    case ILet(lhs, expr)               => ???
    case IAssign(ref, expr)            => ???
    case IExpand(base, expr)           => ???
    case IDelete(base, expr)           => ???
    case IPush(elem, list, front)      => ???
    case IPop(lhs, list, front)        => ???
    case IReturn(expr)                 => ???
    case IAssert(expr)                 => ???
    case IPrint(expr)                  => ???
    case INop()                        => Nil
    case IIf(cond, thenInst, elseInst) => ???
    case IWhile(cond, body)            => ???
    case ICall(lhs, fexpr, args)       => ???
    case ISdoCall(lhs, base, op, args) => ???
    case ISeq(insts)                   => ???

  def peval(func: Func): Func = func

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/log"))
}
