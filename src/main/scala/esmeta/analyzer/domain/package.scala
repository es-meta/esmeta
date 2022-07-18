package esmeta.analyzer.domain

import esmeta.interp.*

// ////////////////////////////////////////////////////////////////////////////
// shortcuts
// ////////////////////////////////////////////////////////////////////////////
lazy val T = Bool(true)
lazy val F = Bool(false)
lazy val AB = AbsBool.Top
lazy val AT = AbsBool(Bool(true))
lazy val AF = AbsBool(Bool(false))
lazy val AVB = AbsValue(bool = AB)
lazy val AVT = AbsValue(bool = AT)
lazy val AVF = AbsValue(bool = AF)

// ////////////////////////////////////////////////////////////////////////////
// Abstract Domains for Values
// ////////////////////////////////////////////////////////////////////////////

// states

lazy val AbsObj = BasicObjDomain
type AbsObj = AbsObj.Elem

// values
lazy val AbsValue = BasicValueDomain
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

lazy val AbsBool = FlatBoolDomain
type AbsBool = AbsBool.Elem

lazy val AbsUndef = SimpleDomain(Undef)
type AbsUndef = AbsUndef.Elem

lazy val AbsNull = SimpleDomain(Null)
type AbsNull = AbsNull.Elem

lazy val AbsAbsent = SimpleDomain(Absent)
type AbsAbsent = AbsAbsent.Elem
