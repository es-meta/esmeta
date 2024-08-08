package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*

/** Partial Value - Either Expr or Value */
final case class PValue(val ty: ValueTy, val expr: Expr):
  // Use a type with a finer granularity than Expr for absExpr

  def knownValue: Option[Value] =
    this.ty.getSingle match
      case Many      => None
      case One(elem) => Some(elem)
      case Zero      => None

  /** to print */
  lazy val asValidExpr: Expr = this.expr

/** Partial RefTarget - Either Ref or RefTarget */
enum PRefTarget:
  case R(absref: Ref, ref: Ref) extends PRefTarget
  case RT(target: RefTarget, ref: Ref) extends PRefTarget

  def knownTarget: Option[RefTarget] = this match
    case _: R   => None
    case rt: RT => Some(rt.target)

  lazy val asValidRef: Ref = this match
    case R(_, ref) => ref
    case RT(tgt, ref) =>
      tgt match
        case VarTarget(x)             => x
        case FieldTarget(base, field) => ???

extension (value: Value)
  def asLitExpr: Expr = value match
    case NamedAddr(name)                 => ???
    case DynamicAddr(long)               => ???
    case Clo(func, captured)             => ???
    case Cont(func, captured, callStack) => ???
    case AstValue(ast)                   => ???
    case GrammarSymbol(name, params)     => ???
    case Math(m)                         => EMath(m)
    case Infinity(pos)                   => EInfinity(pos)
    case Enum(n)                         => EEnum(n)
    case CodeUnit(c)                     => ECodeUnit(c)
    case Number(d)                       => ENumber(d)
    case BigInt(n)                       => EBigInt(n)
    case Str(s)                          => EStr(s)
    case Bool(b)                         => EBool(b)
    case Undef                           => EUndef()
    case Null                            => ENull()

  def toPValue: PValue = value match
    case addr: Addr                      => ???
    case Clo(func, captured)             => ???
    case Cont(func, captured, callStack) => ???
    case AstValue(ast)                   => ???
    case GrammarSymbol(name, params)     => ???
    case Math(d)                         => PValue(MathT(d), value.asLitExpr)
    case Infinity(p) => PValue(InfinityT(p), value.asLitExpr)
    case Enum(name)  => PValue(EnumT(name), value.asLitExpr)
    case CodeUnit(c) => PValue(CodeUnitT, value.asLitExpr)
    case Number(d)   => PValue(NumberT(Number(d)), value.asLitExpr)
    case BigInt(_)   => PValue(BigIntT, value.asLitExpr)
    case Str(s)      => PValue(StrT(s), value.asLitExpr)
    case Bool(b)     => PValue(BoolT(b), value.asLitExpr)
    case Undef       => PValue(UndefT, value.asLitExpr)
    case Null        => PValue(NullT, value.asLitExpr)
