package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*

/** Partial Value - Either Expr or Value */
enum PValue:
  // Use a type with a finer granularity than Expr for absExpr
  case E(absExpr: Expr, e: Expr)
  case V(value: Value, e: Expr)

  def knownValue: Option[Value] = this match
    case _: E => None
    case v: V => Some(v.value)

  /** to print */
  lazy val asValidExpr: Expr = this match
    case E(a, e) => e
    case V(v, e) => e // ?

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

  def toPValue: PValue = PValue.V(value, value.asLitExpr)
