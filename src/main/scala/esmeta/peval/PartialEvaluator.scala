package esmeta.peval

import esmeta.{IRPEVAL_LOG_DIR, TEST_MODE}
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.error.NotSupported.Category.Metalanguage
import esmeta.ir.*
import esmeta.peval.domain.*
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import esmeta.ty.*
import esmeta.interpreter.{Interpreter}
import esmeta.ty.ValueTopTy.infinity

class PartialEvaluator(
  val pst: PState,
  val cfg: CFG,
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
    // case (r : PartialRef.R, e : PValue.V ) => PartialRef.R(Field(r, e), )
    // case _ => ???
    //  Field(peval(base), peval(expr))
    case v: Var => PRefTarget.RT(VarTarget(v), ref)

  def peval(expr: Expr): PValue =
    if (log) then
      pw.print(s"[peval][Expr] $expr")
      pw.flush()
    expr match
      case EParse(code, rule)           => PValue(ValueTopTy, expr)
      case EGrammarSymbol(name, params) => PValue(ValueTopTy, expr)
      case ESourceText(e)               => PValue(ValueTopTy, expr)
      case EYet(msg)                    => PValue(BotT, expr)
      case EContains(list, e)           => PValue(ValueTopTy, expr)
      case ESubstring(e, from, to)      => PValue(ValueTopTy, expr)
      case ETrim(e, isStarting)         => PValue(ValueTopTy, expr)
      case ERef(ref) =>
        val pref = peval(ref)
        pref match
          case PRefTarget.R(absref, ref)  => ???
          case PRefTarget.RT(target, ref) => pst(target)

      case EUnary(uop, e) =>
        val pv = peval(e)
        pv.knownValue match
          case None =>
            PValue(
              ValueTopTy,
              EUnary(uop, pv.asValidExpr),
            )
          case Some(v) => Interpreter.eval(uop, v).toPValue

      case EBinary(bop, left, right) =>
        val pleft = peval(left)
        val pright = peval(right)
        (pleft.knownValue, pright.knownValue) match
          case (Some(v1), Some(v2)) =>
            val v = Interpreter.eval(bop, v1, v2)
            v.toPValue
          case (p1, p2) =>
            val newExpr = EBinary(bop, pleft.asValidExpr, pright.asValidExpr)
            PValue(ValueTopTy, newExpr)

      case EVariadic(vop, es) => PValue(ValueTopTy, expr)
      case EMathOp(mop, args) => PValue(ValueTopTy, expr)
      case EConvert(cop, e)   => PValue(ValueTopTy, expr)

      case EExists(ref) => PValue(BoolT, expr)
      case ETypeOf(base) =>
        val pbase @ PValue(ty, _) = peval(base)
        val tyStr = ty match
          case _ if ty <= NumberT => Some("Number")
          case _ if ty <= BigIntT => Some("BigInt")
          case _ if ty <= StrT    => Some("String")
          case _ if ty <= BoolT   => Some("Boolean")
          case _ if ty <= UndefT  => Some("Undefined")
          case _ if ty <= NullT   => Some("Null")
          case v                  => None
        // if (ObjectT.contains(v, st)) "Object"
        // else if (SymbolT.contains(v, st)) "Symbol"
        // else "SpecType"
        val t = tyStr.map(StrT(_)).getOrElse(StrT)
        PValue(t, pbase.asValidExpr)

      case EInstanceOf(base, target) => PValue(ValueTopTy, expr)
      case ETypeCheck(base, ty)      => PValue(ValueTopTy, expr)
      case ESizeOf(base)             => PValue(ValueTopTy, expr)
      case EClo(fname, captured)     => PValue(CloT, expr)
      case ECont(fname)              => PValue(ContT, expr)
      case EDebug(expr)              => peval(expr)
      case ERandom()                 => PValue(NumberT, expr)

      case ESyntactic(name, args, rhsIdx, children) =>
        PValue(ValueTopTy, expr)
      case ELexical(name, expr) => PValue(ValueTopTy, expr)

      case ERecord(tname, fields) =>
        val pfields = for ((f, expr) <- fields) yield f -> peval(expr)
        val addr = pst.allocRecord(
          tname,
          pfields,
        )(using cfg)
        addr.toPValue

      case EMap(pairs) => PValue(ValueTopTy, expr)
      case EList(exprs) =>
        pst.allocList(exprs.map(expr => peval(expr)).toVector).toPValue

      case ECopy(obj)            => PValue(ValueTopTy, expr)
      case EKeys(map, intSorted) => PValue(ValueTopTy, expr)

      // Literals
      case EMath(n)     => Math(n).toPValue
      case EInfinity(p) => Infinity(p).toPValue
      case ENumber(d)   => Number(d).toPValue
      case EBigInt(n)   => BigInt(n).toPValue
      case EStr(str)    => Str(str).toPValue
      case EBool(b)     => Bool(b).toPValue
      case EUndef()     => Undef.toPValue
      case ENull()      => Null.toPValue
      case EEnum(name)  => Enum(name).toPValue
      case ECodeUnit(c) => CodeUnit(c).toPValue

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

      case IExpand(base, expr)      => inst.toList
      case IDelete(base, expr)      => inst.toList
      case IPush(elem, list, front) => inst.toList
      case IPop(lhs, list, front)   => inst.toList
      case IReturn(expr) =>
        val pexpr = peval(expr)
        List(IReturn(pexpr.asValidExpr))

      case IAssert(expr) => inst.toList
      case IPrint(expr) =>
        val pv = peval(expr)
        if (!TEST_MODE) println(pv)
        List(IPrint(pv.asValidExpr))
      case INop() => Nil
      case IIf(cond, thenInst, elseInst) =>
        val pcond = peval(cond)
        pcond match
          case PValue(ty, expr) if (ty <= BotT)   => ???
          case PValue(ty, expr) if (ty <= TrueT)  => peval(thenInst)
          case PValue(ty, expr) if (ty <= FalseT) => peval(elseInst)
          case PValue(ty, expr) if (ty <= BoolT)  =>
            // TODO : Properly handle mutable state
            List(
              IIf(
                pcond.asValidExpr,
                peval(thenInst).toInst,
                peval(elseInst).toInst,
              ),
            )
          case _ => ???

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

  extension (func: Func) def inlinedBody: Inst = ???

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/func/${targetFunc.name}"))
}

object PartialEvaluator {
  def apply(
    cfg: CFG,
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
          cfg,
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
