package esmeta.peval

// TODO sort import

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
import esmeta.parser.ESValueParser

class PartialEvaluator(
  val pst: PState,
  val pc: PathCondition,
  val cfg: CFG,
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {

  /** resulting function */
  lazy val result: Func = peval(targetFunc)

  def and(expr: Expr): PartialEvaluator =
    new PartialEvaluator(
      pst.clone,
      pc.and(expr),
      cfg,
      prog,
      targetFunc,
      log,
      output,
      logPW,
    )

  def peval(ref: Ref): PRefTarget = ref match
    case v: Var => PRefTarget(PRef.PVar(v), ref)
    case Field(base, expr) =>
      val pbase = peval(base)
      pbase.knownTarget match
        case None      => ???
        case Some(tgt) => PRefTarget(PRef.PField(pst(tgt), peval(expr)), ref)

  def peval(expr: Expr): PValue =
    if (log) then
      pw.print(s"[peval][Expr] $expr")
      pw.flush()
    expr match
      case EParse(code, rule) => PValue(PVTy.Top, expr)

      case EGrammarSymbol(name, params) => PValue(PVTy.Top, expr)

      case ESourceText(e) => PValue(PVTy.Top, expr)

      case EYet(msg) => PValue(PVTy.Bot, expr)

      case EContains(list, e) => PValue(PVTy.Top, expr)

      case ESubstring(e, from, to) => PValue(PVTy.Top, expr)

      case ETrim(e, isStarting) =>
        val pv = peval(e)
        pv.knownValue match
          case None => PValue(PVTy.StrT, pv.asValidExpr)
          case Some(value) =>
            Str(trimString(value.asStr, isStarting, cfg.esParser)).toPValue

      case ERef(ref) =>
        val pt = peval(ref)
        pt.knownTarget match
          case None        => ???
          case Some(value) => pst(value)

      case EUnary(uop, e) =>
        val pv = peval(e)
        pv.knownValue match
          case None =>
            PValue(
              PVTy.Top,
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
            PValue(PVTy.Top, newExpr)

      case EVariadic(vop, es) =>
        val pvs = es.map(peval)
        (pvs.forall(_.knownValue.isDefined)) match
          case true =>
            val v = Interpreter.eval(vop, pvs.map(_.knownValue.get))
            v.toPValue
          case false =>
            PValue(
              pvs.map(_.ty).fold(PVTy.Bot)((t, u) => t ⊔ u),
              EVariadic(vop, pvs.map(_.asValidExpr)),
            )

      case EMathOp(mop, es) =>
        val pvs = es.map(peval)
        (pvs.forall(_.knownValue.isDefined)) match
          case true =>
            val v = Interpreter.eval(mop, pvs.map(_.knownValue.get))
            v.toPValue
          case false =>
            PValue(
              pvs.map(_.ty).fold(PVTy.Bot)((t, u) => t ⊔ u),
              EMathOp(mop, pvs.map(_.asValidExpr)),
            )

      case EConvert(cop, e) =>
        import COp.*
        val pv = peval(e)
        pv.knownValue match
          case None => ???
          case Some(v) =>
            (v, cop) match {
              // code unit
              case (CodeUnit(c), ToMath) => Math(c.toInt).toPValue
              // extended mathematical value
              case (Infinity(true), ToNumber)  => NUMBER_POS_INF.toPValue
              case (Infinity(false), ToNumber) => NUMBER_NEG_INF.toPValue
              case (Math(n), ToApproxNumber)   => Number(n.toDouble).toPValue
              case (Math(n), ToNumber)         => Number(n.toDouble).toPValue
              case (Math(n), ToBigInt)         => BigInt(n.toBigInt).toPValue
              case (Math(n), ToMath)           => Math(n).toPValue
              // string
              case (Str(s), ToNumber) => ESValueParser.str2number(s).toPValue
              case (Str(s), ToBigInt) => ESValueParser.str2bigint(s).toPValue
              case (Str(s), _: ToStr) => Str(s).toPValue
              // numbers
              case (Number(d), ToMath) => Math(d).toPValue
              case (Number(d), ToStr(radixOpt)) =>
                val radix = radixOpt.fold(Math(10).toPValue)(e => peval(e))
                radix.knownValue match
                  case None => ???
                  case Some(radix) =>
                    Str(toStringHelper(d, radix.asInt)).toPValue
              case (Number(d), ToNumber) => Number(d).toPValue
              case (Number(n), ToBigInt) =>
                BigInt(BigDecimal.exact(n).toBigInt).toPValue
              // big integer
              case (BigInt(n), ToMath) => Math(n).toPValue
              case (BigInt(n), ToStr(radixOpt)) =>
                val radix = radixOpt.fold(Math(10).toPValue)(e => peval(e))
                radix.knownValue match
                  case None        => ???
                  case Some(radix) => Str(n.toString(radix.asInt)).toPValue
              case (BigInt(n), ToBigInt) => BigInt(n).toPValue
              // invalid cases
              case (v, cop) => throw InvalidConversion(cop, expr, v)
            }

      case EExists(ref) => PValue(PVTy.BoolT, expr)
      case ETypeOf(base) =>
        val pbase @ PValue(ty, _) = peval(base)
        val tyStr = ty match
          case _ if ty <= PVTy.NumberT => Some("Number")
          case _ if ty <= PVTy.BigIntT => Some("BigInt")
          case _ if ty <= PVTy.StrT    => Some("String")
          case _ if ty <= PVTy.BoolT   => Some("Boolean")
          case _ if ty <= PVTy.UndefT  => Some("Undefined")
          case _ if ty <= PVTy.NullT   => Some("Null")
          case v                       => None
        // if (ObjectT.contains(v, st)) "Object"
        // else if (SymbolT.contains(v, st)) "Symbol"
        // else "SpecType"
        ???
      // val t = tyStr.map(StrT(_)).getOrElse(StrT)
      // PValue(t, pbase.asValidExpr)

      case EInstanceOf(base, target) => PValue(PVTy.Top, expr)
      case ETypeCheck(base, ty)      => PValue(PVTy.Top, expr)
      case ESizeOf(base)             => PValue(PVTy.Top, expr)
      case EClo(fname, captured)     => ??? // PValue(CloT, expr)
      case ECont(fname)              => ??? // PValue(ContT, expr)

      case EDebug(e) => peval(e)

      case ERandom() => PValue(PVTy.NumberT, expr)

      case ESyntactic(name, args, rhsIdx, children) => PValue(PVTy.Top, expr)
      case ELexical(name, e)                        => PValue(PVTy.Top, expr)

      case ERecord(tname, fields) =>
        val pfields = for ((f, expr) <- fields) yield f -> peval(expr)
        val addr = pst.allocRecord(
          tname,
          pfields,
        )(using cfg)
        addr.toPValue

      case EMap(pairs) => PValue(PVTy.Top, expr)
      case EList(exprs) =>
        pst.allocList(exprs.map(expr => peval(expr)).toVector).toPValue

      case ECopy(obj)            => ???
      case EKeys(map, intSorted) => PValue(PVTy.Top, expr)

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
        val pt = peval(ref)
        val pv = peval(expr)
        pt.knownTarget match
          case None => ???
          case Some(tgt) =>
            pst.update(tgt, pv)
            List(IAssign(pt.asValidRef, pv.asValidExpr))

      case IExpand(base, expr) =>
        val pb = peval(base)
        val pf = peval(expr)
        ???

      case IDelete(base, expr) =>
        val pb = peval(base)
        val pf = peval(expr)
        ???

      case IPush(elem, list, front) =>
        val pe = peval(elem)
        val pl = peval(list)
        ???

      case IPop(lhs, list, front) =>
        val pl = peval(list)
        ???

      case IReturn(expr) =>
        val pexpr = peval(expr)
        List(IReturn(pexpr.asValidExpr))

      case IAssert(expr) =>
        // TODO : Add path condition after iassert
        val pv = peval(expr)
        ???

      case IPrint(expr) =>
        val pv = peval(expr)
        if (!TEST_MODE) println(pv)
        List(IPrint(pv.asValidExpr))
      case INop() => Nil
      case IIf(cond, thenInst, elseInst) =>
        val pcond = peval(cond)
        pcond match
          case PValue(ty, expr) if (ty <= PVTy.Bot)    => ???
          case PValue(ty, expr) if (ty <= PVTy.TrueT)  => peval(thenInst)
          case PValue(ty, expr) if (ty <= PVTy.FalseT) => peval(elseInst)
          case PValue(ty, expr) if (ty <= PVTy.BoolT)  =>
            // TODO : Add path condition
            // TODO : Properly handle mutable state
            List(
              IIf(
                pcond.asValidExpr,
                peval(thenInst).toInst,
                peval(elseInst).toInst,
              ),
            )
          case _ => ???

      case IWhile(cond, body) => inst.toList
      case ICall(lhs, fexpr, args) =>
        val f = peval(fexpr)
        f.knownValue match
          case None                            => ???
          case Some(clo @ Clo(func, captured)) => ???
          case Some(value)                     => ???

      case ISdoCall(lhs, base, op, args) => inst.toList

      case ISeq(insts) => ISeq(insts.flatMap(peval)).toList

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
          PathCondition(),
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
