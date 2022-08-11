package esmeta.analyzer.domain

import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.js
import esmeta.error.NotSupportedOperation

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////
def notSupported(obj: Any, method: String): Nothing =
  throw NotSupportedOperation(obj, method)

////////////////////////////////////////////////////////////////////////////////
// Shortcuts
////////////////////////////////////////////////////////////////////////////////
lazy val T = Bool(true)
lazy val F = Bool(false)
lazy val AB = AbsBool.Top
lazy val AT = AbsBool(Bool(true))
lazy val AF = AbsBool(Bool(false))
lazy val AVT = AbsValue(true)
lazy val AVF = AbsValue(false)
lazy val AVB = AVT ⊔ AVF
lazy val AV_TYPE = AbsValue("Type")
lazy val AV_VALUE = AbsValue("Value")
lazy val AV_TARGET = AbsValue("Target")

////////////////////////////////////////////////////////////////////////////////
// Abstract Domains for Values
////////////////////////////////////////////////////////////////////////////////
// TODO refactoring with error messages
// TODO more comments
// global cfg for domain
var _cfgOpt: Option[CFG] = None
lazy val cfg: CFG = _cfgOpt.get // global cfg must be initialized

// states
lazy val AbsRet = BasicRetDomain
type AbsRet = AbsRet.Elem

lazy val AbsState = ABS_STATE
type AbsState = AbsState.Elem

lazy val AbsHeap = BasicHeapDomain
type AbsHeap = AbsHeap.Elem

lazy val AbsObj = BasicObjDomain
type AbsObj = AbsObj.Elem

// values
lazy val AbsValue = ABS_VALUE
type AbsValue = AbsValue.Elem

lazy val AbsComp = BasicCompDomain
type AbsComp = AbsComp.Elem

// pure values

lazy val AbsClo = BasicCloDomain
type AbsClo = AbsClo.Elem

lazy val AbsCont = BasicContDomain
type AbsCont = AbsCont.Elem

lazy val AbsLoc = SetDomain[Loc]("#")
type AbsLoc = AbsLoc.Elem

lazy val AbsAst = FlatDomain[AAst]("ast", isExploded = true)
type AbsAst = AbsAst.Elem

lazy val AbsGrammar = FlatDomain[AGrammar]("grammar")
type AbsGrammar = AbsGrammar.Elem

lazy val AbsCodeUnit = FlatDomain[ACodeUnit]("codeunit")
type AbsCodeUnit = AbsCodeUnit.Elem

lazy val AbsConst = FlatDomain[AConst]("const")
type AbsConst = AbsConst.Elem

lazy val AbsMath = FlatDomain[AMath]("math")
type AbsMath = AbsMath.Elem

// simple values
lazy val AbsSimple = BasicSimpleValueDomain
type AbsSimple = AbsSimple.Elem

lazy val AbsNum = FlatDomain[Number]("number")
type AbsNum = AbsNum.Elem

lazy val AbsBigInt = FlatDomain[BigInt]("bigint")
type AbsBigInt = AbsBigInt.Elem

lazy val AbsStr = SetDomain[Str]("str")
type AbsStr = AbsStr.Elem

var ABS_BOOL = FlatBoolDomain
lazy val AbsBool: BoolDomain = ABS_BOOL
type AbsBool = AbsBool.Elem

lazy val AbsUndef = SimpleDomain(Undef)
type AbsUndef = AbsUndef.Elem

lazy val AbsNull = SimpleDomain(Null)
type AbsNull = AbsNull.Elem

lazy val AbsAbsent = SimpleDomain(Absent)
type AbsAbsent = AbsAbsent.Elem

// parametric domains
var ABS_STATE: StateDomain = BasicStateDomain
var ABS_VALUE: ValueDomain = BasicValueDomain
