package esmeta.ai.domain

import esmeta.state.Bool

// -----------------------------------------------------------------------------
// shortcuts
// -----------------------------------------------------------------------------
val T = Bool(true)
val F = Bool(false)

// -----------------------------------------------------------------------------
// domain types
// -----------------------------------------------------------------------------
type StateDomain = state.Domain
// type RetDomain = ret.Domain
// type HeapDomain = heap.Domain
// type ObjDomain = obj.Domain
type ValueDomain = value.Domain
// type CompDomain = comp.Domain
type PureValueDomain = pureValue.Domain
type CloDomain = clo.Domain
type ContDomain = cont.Domain
type AddrDomain = addr.Domain
type AstValueDomain = astValue.Domain
type GrammarDomain = grammar.Domain
type MathDomain = math.Domain
type CodeUnitDomain = codeUnit.Domain
type ConstDomain = const.Domain
type SimpleValueDomain = simpleValue.Domain
type NumberDomain = number.Domain
type BigIntDomain = bigInt.Domain
type StrDomain = str.Domain
type BoolDomain = bool.Domain
type UndefDomain = undef.Domain
type NullDomain = nullv.Domain
type AbsentDomain = absent.Domain

// -----------------------------------------------------------------------------
// domain factory types
// -----------------------------------------------------------------------------
type StateFactory = Config => StateDomain
// type RetFactory = Config => RetDomain
// type HeapFactory = Config => HeapDomain
// type ObjFactory = Config => ObjDomain
type ValueFactory = Config => ValueDomain
// type CompFactory = Config => CompDomain
type PureValueFactory = Config => PureValueDomain
type CloFactory = Config => CloDomain
type ContFactory = Config => ContDomain
type AddrFactory = Config => AddrDomain
type AstValueFactory = Config => AstValueDomain
type GrammarFactory = Config => GrammarDomain
type MathFactory = Config => MathDomain
type CodeUnitFactory = Config => CodeUnitDomain
type ConstFactory = Config => ConstDomain
type SimpleValueFactory = Config => SimpleValueDomain
type NumberFactory = Config => NumberDomain
type BigIntFactory = Config => BigIntDomain
type StrFactory = Config => StrDomain
type BoolFactory = Config => BoolDomain
type UndefFactory = Config => UndefDomain
type NullFactory = Config => NullDomain
type AbsentFactory = Config => AbsentDomain
