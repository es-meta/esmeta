package esmeta.peval.domain

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.{BoolTy, InfinityTy, MathSetTy, NumberSetTy}
import esmeta.util.*

case class PRefTarget(val tgt: ARefTarget, private val ref: Ref)
  extends PartialElem[RefTarget, Ref]:
  def known: Option[RefTarget] = this.tgt match
    case ARefTarget.AVarTarget(x) => Some(VarTarget(x))
    case ARefTarget.AFieldTarget(base, field) =>
      (base.known, field.known) match
        case (Some(base), Some(field)) => Some(FieldTarget(base, field))
        case _                         => None

  lazy val asValidForm: Ref = ref

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

  def toAValue: AValue = value match
    case addr: Addr                      => AValue(addr = Fin(addr))
    case Clo(func, captured)             => ???
    case Cont(func, captured, callStack) => ???
    case AstValue(ast)                   => ???
    case GrammarSymbol(name, params)     => ???
    case Math(d)     => AValue(math = MathSetTy(Set(Math(d))))
    case Infinity(p) => AValue(infinity = InfinityTy(Set(p)))
    case Enum(name)  => AValue(enumv = Fin(name))
    case CodeUnit(c) => AValue(codeUnit = true)
    case Number(d)   => AValue(number = NumberSetTy(Set(Number(d))))
    case BigInt(_)   => AValue(bigInt = true)
    case Str(s)      => AValue(str = Fin(s))
    case Bool(b)     => AValue(bool = BoolTy(Set(b)))
    case Undef       => AValue(undef = true)
    case Null        => AValue(nullv = true)

  def toPValue: PValue = value match
    case addr: Addr          => ??? // PValue(AValue(addr = Fin(addr)))
    case Clo(func, captured) => ???
    case Cont(func, captured, callStack) => ???
    case AstValue(ast)                   => ???
    case GrammarSymbol(name, params)     => ???
    case _ => PValue(value.toAValue, value.asLitExpr)

  def toPValueWithVar(v: Var) = PValue(value.toAValue, ERef(v))
