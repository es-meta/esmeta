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

type RetDomain = ret.Domain
lazy val AbsRet: RetDomain = Config.retDomain
type AbsRet = AbsRet.Elem

type HeapDomain = heap.Domain
lazy val AbsHeap: HeapDomain = Config.heapDomain
type AbsHeap = AbsHeap.Elem

type ObjDomain = obj.Domain
lazy val AbsObj: ObjDomain = Config.objDomain
type AbsObj = AbsObj.Elem

type ValueDomain = value.Domain
lazy val AbsValue: ValueDomain = Config.valueDomain
type AbsValue = AbsValue.Elem

type CompDomain = comp.Domain
lazy val AbsComp: CompDomain = Config.compDomain
type AbsComp = AbsComp.Elem

type PureValueDomain = pureValue.Domain
lazy val AbsPureValue: PureValueDomain = Config.pureValueDomain
type AbsPureValue = AbsPureValue.Elem

type CloDomain = clo.Domain
lazy val AbsClo: CloDomain = Config.cloDomain
type AbsClo = AbsClo.Elem

type ContDomain = cont.Domain
lazy val AbsCont: ContDomain = Config.contDomain
type AbsCont = AbsCont.Elem

type PartDomain = part.Domain
lazy val AbsPart: PartDomain = Config.partDomain
type AbsPart = AbsPart.Elem

type AstValueDomain = astValue.Domain
lazy val AbsAstValue: AstValueDomain = Config.astValueDomain
type AbsAstValue = AbsAstValue.Elem

type GrammarDomain = grammar.Domain
lazy val AbsGrammar: GrammarDomain = Config.grammarDomain
type AbsGrammar = AbsGrammar.Elem

type MathDomain = math.Domain
lazy val AbsMath: MathDomain = Config.mathDomain
type AbsMath = AbsMath.Elem

type CodeUnitDomain = codeUnit.Domain
lazy val AbsCodeUnit: CodeUnitDomain = Config.codeUnitDomain
type AbsCodeUnit = AbsCodeUnit.Elem

type ConstDomain = const.Domain
lazy val AbsConst: ConstDomain = Config.constDomain
type AbsConst = AbsConst.Elem

type SimpleValueDomain = simpleValue.Domain
lazy val AbsSimpleValue: SimpleValueDomain = Config.simpleValueDomain
type AbsSimpleValue = AbsSimpleValue.Elem

type NumberDomain = number.Domain
lazy val AbsNumber: NumberDomain = Config.numberDomain
type AbsNumber = AbsNumber.Elem

type BigIntDomain = bigInt.Domain
lazy val AbsBigInt: BigIntDomain = Config.bigIntDomain
type AbsBigInt = AbsBigInt.Elem

type StrDomain = str.Domain
lazy val AbsStr: StrDomain = Config.strDomain
type AbsStr = AbsStr.Elem

type BoolDomain = bool.Domain
lazy val AbsBool: BoolDomain = Config.boolDomain
type AbsBool = AbsBool.Elem

type UndefDomain = undef.Domain
lazy val AbsUndef: UndefDomain = Config.undefDomain
type AbsUndef = AbsUndef.Elem

type NullDomain = nullv.Domain
lazy val AbsNull: NullDomain = Config.nullDomain
type AbsNull = AbsNull.Elem

type AbsentDomain = absent.Domain
lazy val AbsAbsent: AbsentDomain = Config.absentDomain
type AbsAbsent = AbsAbsent.Elem
