package esmeta.peval

// TODO sort import

import esmeta.{IRPEVAL_LOG_DIR, TEST_MODE}
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.error.NotSupported.Category.Metalanguage
import esmeta.ir.*
import esmeta.interpreter.{Interpreter}
import esmeta.parser.ESValueParser
import esmeta.peval.domain.*
import esmeta.peval.pstate.*
import esmeta.peval.simplifier.*
import esmeta.state.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.ty.*
import esmeta.ty.ValueTopTy.infinity
import java.io.PrintWriter

class PartialEvaluator(
  val pst: PState,
  val cfg: CFG,
  val prog: Program,
  val targetFunc: Func,
  val log: Boolean = false,
  val output: Option[String] = None,
  val logPW: Option[PrintWriter] = None,
) {
  import PartialEvaluator.*

  /** resulting function */
  lazy val result: Func = peval(targetFunc)

  def peval(ref: Ref)(using TempAllocator): PRefTarget = ref match
    case v: Var => PRefTarget(ARefTarget.AVarTarget(v), ref)
    case Field(base, expr) =>
      val pbase = peval(base)
      val pexpr = peval(expr)
      PRefTarget(
        ARefTarget.AFieldTarget(pst(pbase), pexpr),
        Field(pbase.asValidForm, pexpr.asValidForm),
      )

  def peval(expr: Expr)(using tempAllocator: TempAllocator): PValue =
    if (log) then
      pw.print(s"[peval][Expr] $expr")
      pw.flush()
    expr match
      case EParse(code, rule)           => { PValue(AValue.Top, expr) }
      case EGrammarSymbol(name, params) => { PValue(AValue.Top, expr) }
      case ESourceText(e)               => { PValue(AValue.Top, expr) }
      case EYet(msg)                    => { PValue(AValue.Bot, expr) }

      case EContains(list, e) => {
        val pl = peval(list)
        // for (addr <- pl.addr) ()
        ???
      }

      case ESubstring(e, from, to) => { PValue(AValue.Top, expr) }

      case ETrim(e, isStarting) => {
        val pv = peval(e)
        pv.known match
          case None => PValue(AValue.StrT, pv.asValidForm)
          case Some(value) =>
            Str(trimString(value.asStr, isStarting, cfg.esParser)).toPValue
      }
      case ERef(ref) => {
        val pt = peval(ref)
        pt.known match
          case None        => ???
          case Some(value) => { pst(value) }
      }
      case EUnary(uop, e) => {
        val pv = peval(e)
        pv.known match
          case None =>
            PValue(
              AValue.Top,
              EUnary(uop, pv.asValidForm),
            )
          case Some(v) => Interpreter.eval(uop, v).toPValue
      }

      case EBinary(bop, left, right) => {
        val pleft = peval(left)
        val pright = peval(right)
        (pleft.known, pright.known) match
          case (Some(v1), Some(v2)) =>
            val v = Interpreter.eval(bop, v1, v2)
            v.toPValue
          case (p1, p2) =>
            val newExpr = EBinary(bop, pleft.asValidForm, pright.asValidForm)
            PValue(AValue.Top, newExpr)
      }

      case EVariadic(vop, es) => {
        val pvs = es.map(peval)
        (pvs.forall(_.known.isDefined)) match
          case true =>
            val v = Interpreter.eval(vop, pvs.map(_.known.get))
            v.toPValue
          case false =>
            PValue(
              pvs.map(_.ty).fold(AValue.Bot)((t, u) => t ⊔ u),
              EVariadic(vop, pvs.map(_.asValidForm)),
            )
      }

      case EMathOp(mop, es) => {
        val pvs = es.map(peval)
        (pvs.forall(_.known.isDefined)) match
          case true =>
            val v = Interpreter.eval(mop, pvs.map(_.known.get))
            v.toPValue
          case false =>
            PValue(
              pvs.map(_.ty).fold(AValue.Bot)((t, u) => t ⊔ u),
              EMathOp(mop, pvs.map(_.asValidForm)),
            )
      }

      case EConvert(cop, e) => {
        import COp.*
        val pv = peval(e)
        pv.known match
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
                radix.known match
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
                radix.known match
                  case None        => ???
                  case Some(radix) => Str(n.toString(radix.asInt)).toPValue
              case (BigInt(n), ToBigInt) => BigInt(n).toPValue
              // invalid cases
              case (v, cop) => throw InvalidConversion(cop, expr, v)
            }
      }

      case EExists(ref) => ??? // pst.exists(peval(ref))
      case ETypeOf(base) =>
        val pbase @ PValue(ty, _) = peval(base)
        val tyStr = ty match
          case _ if ty <= AValue.NumberT => Some("Number")
          case _ if ty <= AValue.BigIntT => Some("BigInt")
          case _ if ty <= AValue.StrT    => Some("String")
          case _ if ty <= AValue.BoolT   => Some("Boolean")
          case _ if ty <= AValue.UndefT  => Some("Undefined")
          case _ if ty <= AValue.NullT   => Some("Null")
          case v                         => None
        // if (ObjectT.contains(v, st)) "Object"
        // else if (SymbolT.contains(v, st)) "Symbol"
        // else "SpecType"
        ???
      // val t = tyStr.map(StrT(_)).getOrElse(StrT)
      // PValue(t, pbase.asValidExpr)

      case EInstanceOf(base, target) => PValue(AValue.Top, expr)

      case ETypeCheck(base, ty) => PValue(AValue.Top, expr)

      case ESizeOf(base) => PValue(AValue.Top, expr)

      case EClo(fname, captured) => 

      case ECont(fname)          => throw NotSupportedSyntax(expr)

      case EDebug(e) => debug(peval(e))

      case ERandom() =>
        val tmp = tempAllocator.get
        tempAllocator.addIAssign(
          IAssign(tmp, ERandom()),
        )
        PValue(AValue.NumberT, ERef(tmp))

      case ESyntactic(name, args, rhsIdx, children) => PValue(AValue.Top, expr)
      case ELexical(name, e)                        => PValue(AValue.Top, expr)

      case ERecord(tname, fields) =>
        val pfields = for ((f, expr) <- fields) yield f -> peval(expr)
        val addr = pst.allocRecord(
          tname,
          pfields,
        )(using cfg)
        val tmp = tempAllocator.get
        tempAllocator.addIAssign(
          IAssign(tmp, ERecord(tname, pfields.map(_ -> _.asValidForm))),
        )
        addr.toPValueWithVar(tmp)

      case EMap(pairs) =>
        val ppairs = for ((k, v) <- pairs) yield peval(k) -> peval(v)
        val addr = pst.allocMap(???)
        val tmp = tempAllocator.get
        tempAllocator.addIAssign(
          IAssign(tmp, EMap(ppairs.map(_.asValidForm -> _.asValidForm))),
        )
        addr.toPValueWithVar(tmp)

      case EList(exprs) =>
        val list = exprs.map(expr => peval(expr))
        val tmp = tempAllocator.get
        tempAllocator.addIAssign(
          IAssign(tmp, EList(list.map(_.asValidForm))),
        )
        val addr = pst.allocList(list.toVector)
        addr.toPValueWithVar(tmp)

      case ECopy(obj)            => ???
      case EKeys(map, intSorted) => PValue(AValue.Top, expr)

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

  def peval(inst: Inst)(using pc: PathCondition): InstPevalResult =
    if (log) then
      pw.print(s"[peval][Inst] $inst")
      pw.close()
    inst match {
      case IExpr(expr) => {
        val ta = tempAllocator()
        val pinst = IExpr(peval(expr)(using ta).asValidForm)
        PevalResult(
          ta.flush,
          pinst,
        )
      }

      case ILet(lhs, expr) =>
        val ta = tempAllocator()
        val pexpr = peval(expr)(using ta)
        pst.define(lhs, pexpr)
        PevalResult(ta.flush, ILet(lhs, pexpr.asValidForm))

      case IAssign(ref, expr) =>
        val ta = tempAllocator()
        val pr = peval(ref)(using ta)
        val pv = peval(expr)(using ta)
        pst.update(pr, pv)
        PevalResult(ta.flush, IAssign(pr.asValidForm, pv.asValidForm))

      case IExpand(base, field) =>
        val ta = tempAllocator()
        val pb = peval(base)(using ta)
        val pf = peval(field)(using ta)
        pst.expand(pst(pb), pf)
        PevalResult(ta.flush, IExpand(pb.asValidForm, pf.asValidForm))

      case IDelete(base, field) =>
        val ta = tempAllocator()
        val pb = peval(base)(using ta)
        val pf = peval(field)(using ta)
        pst.delete(pst(pb), pf)
        PevalResult(ta.flush, IDelete(pb.asValidForm, pf.asValidForm))

      case IPush(elem, list, front) =>
        val ta = tempAllocator()
        val pe = peval(elem)(using ta) // val value = eval(elem)
        val pl = peval(list)(using ta) // val addr = eval(list).asAddr
        // ??? : Should check pl <: AddrT?
        // if (!(pl.ty <= AValue.AddrT)) then throw NoAddrT(pl.ty)
        // XXX how to handle multiple addrs .. ?
        for (addr <- pl.addr) pst.push(addr, pe, front)
        PevalResult(ta.flush, IPush(pe.asValidForm, pl.asValidForm, front))

      case IPop(lhs, list, front) =>
        val ta = tempAllocator()
        val pl = peval(list)(using ta)
        val pops = for (addr <- pl.addr) yield pst.pop(addr, front)
        // pops.foldLeft ... (join)
        // XXX how to handle multiple addrs .. ?
        pst.define(lhs, ???)
        PevalResult(ta.flush, IPop(lhs, pl.asValidForm, front))

      case IReturn(expr) => {
        val ta = tempAllocator()
        pst.callStack match {
          case Nil =>
            val pv = peval(expr)(using ta)
            PevalResult(ta.flush, IReturn(pv.asValidForm))

          case cc :: _ =>
            val pv = peval(expr)(using ta)
            // ILet is impossible since type mismatches
            PevalResult(ta.flush, IAssign(cc.retId, pv.asValidForm))
        }
      }

      case IAssert(expr) => {
        val ta = tempAllocator()
        // TODO : Add path condition after iassert
        val pv = peval(expr)(using ta)
        // TODO : Should check `if (not pv <: TrueT ) then throw`?
        PevalResult(ta.flush, IAssert(pv.asValidForm))
      }

      case IPrint(expr) => {
        val ta = tempAllocator()
        val pv = peval(expr)(using ta)
        if (!TEST_MODE) println(pv)
        PevalResult(ta.flush, IPrint(pv.asValidForm))
      }

      case INop() => { PevalResult.emptyInst }

      case IIf(cond, thenInst, elseInst) =>
        val ta = tempAllocator()
        val pcond = peval(cond)(using ta)
        pcond match {
          case _ if (pcond.ty <= AValue.Bot) => ???
          case _ if (pcond.ty <= AValue.TrueT) =>
            peval(thenInst)(using pc.and(pcond.asValidForm))
          case _ if (pcond.ty <= AValue.FalseT) =>
            peval(thenInst)(using pc.and(EUnary(UOp.Not, (pcond.asValidForm))))
          case _ if (pcond.ty <= AValue.BoolT) =>
            // TODO : join state
            ???
          // PevalResult(
          //   IIf(
          //     pcond.asValidForm,
          //     peval(thenInst).toInst,
          //     peval(elseInst).toInst,
          //   ),
          // )
          case _ => ???
        }

      case IWhile(cond, body) => {
        // val pcond = peval(cond)
        ???
      }

      case ICall(lhs, fexpr, args) => {
        val ta = tempAllocator()
        val f = peval(fexpr)(using ta)
        f.known match {
          case None => ???
          case Some(clo @ Clo(cfgFunc, captured)) =>
            PevalResult(
              ta.flush,
              cfgFunc.irFunc.renamedBody,
            )
          case Some(value) => ???
        }
      }

      case ISdoCall(_, _, _, _) => { throw NotSupportedSyntax(inst) }

      case ISeq(insts) =>
        PevalResult {
          val newInsts = insts.match
            case Nil => Nil
            case head :: next =>
              val res = peval(head)
              res.guard match
                case None => res.insts ::: peval(ISeq(next)).insts
                case Some(g) =>
                  res.insts ::: List(
                    IIf(
                      g.guard,
                      ISeq(peval(ISeq(next))(using pc.and(g)).insts),
                      ISeq(Nil),
                    ),
                  )
          newInsts.toSingleInst
        }
    }
  def peval(func: Func): Func = Func(
    func.main,
    func.kind,
    func.name,
    func.params,
    func.retTy,
    peval(func.body)(using PathCondition()).insts.toSingleInst,
    func.algo,
  )

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$IRPEVAL_LOG_DIR/func/${targetFunc.name}"))

  private def tempAllocator(): TempAllocator = TempAllocator(0)
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

  extension (insts: List[Inst]) def toSingleInst: Inst = ISeq(insts)

  extension (func: Func)
    def renamedBody: Inst =
      // TODO rename
      func.body
}
