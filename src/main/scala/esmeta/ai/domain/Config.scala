package esmeta.ai.domain

import esmeta.cfg.*
import esmeta.state.*

class Config(
  cfg: CFG,
  stateFactory: Config => StateDomain = state.BasicDomain(_),
  valueFactory: Config => ValueDomain = value.BasicDomain(_),
  // retFactory: Config => RetDomain,
  // heapFactory: Config => HeapDomain,
  // objFactory: Config => ObjDomain,
  // compFactory: Config => CompDomain,
  // cloFactory: Config => CloDomain,
  // contFactory: Config => ContDomain,
  // locFactory: Config => LocDomain,
  // astFactory: Config => AstDomain,
  // grammarFactory: Config => GrammarDomain,
  // codeUnitFactory: Config => CodeUnitDomain,
  // constFactory: Config => ConstDomain,
  // mathFactory: Config => MathDomain,
  // simpleValueFactory: Config => SimpleValueDomain,
  numberFactory: Config => NumberDomain = _ => FlatDomain("number"),
  bigIntFactory: Config => BigIntDomain = _ => FlatDomain("bigint"),
  strFactory: Config => StrDomain = _ => SetDomain("str", 10),
  boolFactory: Config => BoolDomain = bool.FlatDomain(_),
  undefFactory: Config => UndefDomain = _ => SimpleDomain("undef", Undef),
  nullFactory: Config => NullDomain = _ => SimpleDomain("null", Null),
  absentFactory: Config => AbsentDomain = _ => SimpleDomain("absent", Absent),
) {
  // ---------------------------------------------------------------------------
  // shortcuts
  // ---------------------------------------------------------------------------
  lazy val AB = AbsBool.Top
  lazy val AT = AbsBool(Bool(true))
  lazy val AF = AbsBool(Bool(false))
  // lazy val AVT = AbsValue(true)
  // lazy val AVF = AbsValue(false)
  // lazy val AVB = AVT ⊔ AVF
  // lazy val AV_TYPE = AbsValue("Type")
  // lazy val AV_VALUE = AbsValue("Value")
  // lazy val AV_TARGET = AbsValue("Target")

  // ---------------------------------------------------------------------------
  // abstract domains
  // ---------------------------------------------------------------------------
  final lazy val AbsState: StateDomain = stateFactory(this)
  final lazy val AbsValue: ValueDomain = valueFactory(this)
  // final lazy val AbsRet: RetDomain = retFactory(this)
  // final lazy val AbsHeap: HeapDomain = heapFactory(this)
  // final lazy val AbsObj: ObjDomain = objFactory(this)
  // final lazy val AbsComp: CompDomain = compFactory(this)
  // final lazy val AbsClo: CloDomain = cloFactory(this)
  // final lazy val AbsCont: ContDomain = contFactory(this)
  // final lazy val AbsLoc: LocDomain = locFactory(this)
  // final lazy val AbsAst: AstDomain = astFactory(this)
  // final lazy val AbsGrammar: GrammarDomain = grammarFactory(this)
  // final lazy val AbsCodeUnit: CodeUnitDomain = codeUnitFactory(this)
  // final lazy val AbsConst: ConstDomain = constFactory(this)
  // final lazy val AbsMath: MathDomain = mathFactory(this)
  // final lazy val AbsSimple: SimpleDomain = simpleFactory(this)
  final lazy val AbsNumber: NumberDomain = numberFactory(this)
  final lazy val AbsBigInt: BigIntDomain = bigIntFactory(this)
  final lazy val AbsStr: StrDomain = strFactory(this)
  final lazy val AbsBool: BoolDomain = boolFactory(this)
  final lazy val AbsUndef: UndefDomain = undefFactory(this)
  final lazy val AbsNull: NullDomain = nullFactory(this)
  final lazy val AbsAbsent: AbsentDomain = absentFactory(this)

  // ---------------------------------------------------------------------------
  // element types
  // ---------------------------------------------------------------------------
  // type AbsRet = AbsRet.Elem
  type AbsState = AbsState.Elem
  // type AbsHeap = AbsHeap.Elem
  // type AbsObj = AbsObj.Elem
  type AbsValue = AbsValue.Elem
  // type AbsComp = AbsComp.Elem
  // type AbsClo = AbsClo.Elem
  // type AbsCont = AbsCont.Elem
  // type AbsLoc = AbsLoc.Elem
  // type AbsAst = AbsAst.Elem
  // type AbsGrammar = AbsGrammar.Elem
  // type AbsCodeUnit = AbsCodeUnit.Elem
  // type AbsConst = AbsConst.Elem
  // type AbsMath = AbsMath.Elem
  // type AbsSimple = AbsSimple.Elem
  // type AbsNum = AbsNum.Elem
  // type AbsBigInt = AbsBigInt.Elem
  // type AbsStr = AbsStr.Elem
  type AbsBool = AbsBool.Elem
  type AbsUndef = AbsUndef.Elem
  type AbsNull = AbsNull.Elem
  type AbsAbsent = AbsAbsent.Elem
}
