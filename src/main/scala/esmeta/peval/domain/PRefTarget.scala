package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.{BoolTy, InfinityTy, MathSetTy, NumberSetTy}
import esmeta.util.Fin

case class PRefTarget(val tgt: ARefTarget, private val ref: Ref):

  def knownTarget: Option[RefTarget] = this.tgt match
    case ARefTarget.AVar(x) => Some(VarTarget(x))
    case ARefTarget.AField(base, field) =>
      (base.knownValue, field.knownValue) match
        case (Some(base), Some(field)) => Some(FieldTarget(base, field))
        case _                         => None

  lazy val asValidRef: Ref = ref

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
    case addr: Addr          => PValue(AValue(addr = Fin(addr)), ???)
    case Clo(func, captured) => ???
    case Cont(func, captured, callStack) => ???
    case AstValue(ast)                   => ???
    case GrammarSymbol(name, params)     => ???
    case Math(d) =>
      PValue(AValue(math = MathSetTy(Set(Math(d)))), value.asLitExpr)
    case Infinity(p) =>
      PValue(AValue(infinity = InfinityTy(Set(p))), value.asLitExpr)
    case Enum(name)  => PValue(AValue(enumv = Fin(name)), value.asLitExpr)
    case CodeUnit(c) => PValue(AValue(codeUnit = true), value.asLitExpr)
    case Number(d) =>
      PValue(AValue(number = NumberSetTy(Set(Number(d)))), value.asLitExpr)
    case BigInt(_) => PValue(AValue(bigInt = true), value.asLitExpr)
    case Str(s)    => PValue(AValue(str = Fin(s)), value.asLitExpr)
    case Bool(b)   => PValue(AValue(bool = BoolTy(Set(b))), value.asLitExpr)
    case Undef     => PValue(AValue(undef = true), value.asLitExpr)
    case Null      => PValue(AValue(nullv = true), value.asLitExpr)
