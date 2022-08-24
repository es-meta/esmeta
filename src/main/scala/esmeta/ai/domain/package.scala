package esmeta.ai.domain

import esmeta.state.*

// -----------------------------------------------------------------------------
// shortcuts
// -----------------------------------------------------------------------------
lazy val T = Bool(true)
lazy val F = Bool(false)
lazy val AB = AbsBool.Top
lazy val AT = AbsBool(T)
lazy val AF = AbsBool(F)
lazy val AVT = AbsValue(T)
lazy val AVF = AbsValue(F)
lazy val AVB = AbsValue(T, F)
lazy val AV_TYPE = AbsValue(Str("Type"))
lazy val AV_VALUE = AbsValue(Str("Value"))
lazy val AV_TARGET = AbsValue(Str("Target"))

// -----------------------------------------------------------------------------
// abstract domains
// -----------------------------------------------------------------------------
type StateDomain = state.Domain
lazy val AbsState: StateDomain = Config.stateDomain
type AbsState = AbsState.Elem

// type RetDomain = ret.Domain
// lazy val AbsRet: RetDomain = Config.retDomain
// type AbsRet = AbsRet.Elem

type HeapDomain = heap.Domain
lazy val AbsHeap: HeapDomain = Config.heapDomain
type AbsHeap = AbsHeap.Elem

type ObjDomain = obj.Domain
lazy val AbsObj = Config.objDomain
type AbsObj = AbsObj.Elem

type ValueDomain = value.Domain
lazy val AbsValue = Config.valueDomain
type AbsValue = AbsValue.Elem

type CompDomain = comp.Domain
lazy val AbsComp = Config.compDomain
type AbsComp = AbsComp.Elem

type PureValueDomain = pureValue.Domain
lazy val AbsPureValue = Config.pureValueDomain
type AbsPureValue = AbsPureValue.Elem

type CloDomain = clo.Domain
lazy val AbsClo = Config.cloDomain
type AbsClo = AbsClo.Elem

type ContDomain = cont.Domain
lazy val AbsCont = Config.contDomain
type AbsCont = AbsCont.Elem

type PartDomain = part.Domain
lazy val AbsPart = Config.partDomain
type AbsPart = AbsPart.Elem

type AstValueDomain = astValue.Domain
lazy val AbsAstValue = Config.astValueDomain
type AbsAstValue = AbsAstValue.Elem

type GrammarDomain = grammar.Domain
lazy val AbsGrammar = Config.grammarDomain
type AbsGrammar = AbsGrammar.Elem

type MathDomain = math.Domain
lazy val AbsMath = Config.mathDomain
type AbsMath = AbsMath.Elem

type CodeUnitDomain = codeUnit.Domain
lazy val AbsCodeUnit = Config.codeUnitDomain
type AbsCodeUnit = AbsCodeUnit.Elem

type ConstDomain = const.Domain
lazy val AbsConst = Config.constDomain
type AbsConst = AbsConst.Elem

type SimpleValueDomain = simpleValue.Domain
lazy val AbsSimpleValue = Config.simpleValueDomain
type AbsSimpleValue = AbsSimpleValue.Elem

type NumberDomain = number.Domain
lazy val AbsNumber = Config.numberDomain
type AbsNumber = AbsNumber.Elem

type BigIntDomain = bigInt.Domain
lazy val AbsBigInt = Config.bigIntDomain
type AbsBigInt = AbsBigInt.Elem

type StrDomain = str.Domain
lazy val AbsStr = Config.strDomain
type AbsStr = AbsStr.Elem

type BoolDomain = bool.Domain
lazy val AbsBool = Config.boolDomain
type AbsBool = AbsBool.Elem

type UndefDomain = undef.Domain
lazy val AbsUndef = Config.undefDomain
type AbsUndef = AbsUndef.Elem

type NullDomain = nullv.Domain
lazy val AbsNull = Config.nullDomain
type AbsNull = AbsNull.Elem

type AbsentDomain = absent.Domain
lazy val AbsAbsent = Config.absentDomain
type AbsAbsent = AbsAbsent.Elem
