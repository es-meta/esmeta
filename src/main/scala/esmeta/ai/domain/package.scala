package esmeta.ai.domain

import esmeta.state.*
import esmeta.es.Ast

// shortcuts
val T = Bool(true)
val F = Bool(false)

// domain types
type StateDomain = state.Domain
type ValueDomain = value.Domain
// type RetDomain = Domain[Ret]
type HeapDomain = Domain[Heap]
type ObjDomain = Domain[Obj]
type CompDomain = Domain[Comp]
type CloDomain = Domain[Clo]
type ContDomain = Domain[Cont]
// type LocDomain = Domain[Loc]
type AstDomain = Domain[Ast]
type GrammarDomain = Domain[Grammar]
type CodeUnitDomain = Domain[CodeUnit]
type ConstDomain = Domain[Const]
type MathDomain = Domain[Math]
type SimpleValueDomain = Domain[SimpleValue]
type NumberDomain = Domain[Number]
type BigIntDomain = Domain[BigInt]
type StrDomain = Domain[Str]
type BoolDomain = bool.Domain
type UndefDomain = Domain[Undef]
type NullDomain = Domain[Null]
type AbsentDomain = Domain[Absent]
