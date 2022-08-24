package esmeta.ai.domain

import esmeta.cfg.*
import esmeta.state.*

class Config(
  cfg: CFG,
  stateFactory: StateFactory = state.BasicDomain(_),
  // retFactory: RetFactory,
  // heapFactory: HeapFactory,
  // objFactory: ObjFactory,
  valueFactory: ValueFactory = value.BasicDomain(_),
  // compFactory: CompFactory,
  pureValueFactory: PureValueFactory = pureValue.BasicDomain(_),
  cloFactory: CloFactory = _ => new CloDomain with FlatDomain[Clo]("clo"),
  contFactory: ContFactory = _ => new ContDomain with FlatDomain[Cont]("cont"),
  addrFactory: AddrFactory = _ => addr.Domain(addr.AllocSitePartition),
  astValueFactory: AstValueFactory = _ =>
    new AstValueDomain with FlatDomain[AstValue]("astValue"),
  grammarFactory: GrammarFactory = _ =>
    new GrammarDomain with FlatDomain[Grammar]("grammar"),
  mathFactory: MathFactory = _ => new MathDomain with FlatDomain[Math]("math"),
  codeUnitFactory: CodeUnitFactory = _ =>
    new CodeUnitDomain with FlatDomain[CodeUnit]("codeUnit"),
  constFactory: ConstFactory = _ =>
    new ConstDomain with FlatDomain[Const]("const"),
  simpleValueFactory: SimpleValueFactory = simpleValue.BasicDomain(_),
  numberFactory: NumberFactory = _ =>
    new NumberDomain with FlatDomain[Number]("number"),
  bigIntFactory: BigIntFactory = _ =>
    new BigIntDomain with FlatDomain[BigInt]("bigInt"),
  strFactory: StrFactory = _ =>
    new StrDomain with SetDomain[Str]("str", Some(10)),
  boolFactory: BoolFactory = bool.FlatDomain(_),
  undefFactory: UndefFactory = _ =>
    new UndefDomain with SimpleDomain("undef", Fin(Undef)),
  nullFactory: NullFactory = _ =>
    new NullDomain with SimpleDomain("null", Fin(Null)),
  absentFactory: AbsentFactory = _ =>
    new AbsentDomain with SimpleDomain("absent", Fin(Absent)),
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
  final lazy val AbsState = stateFactory(this)
  // final lazy val AbsRet = retFactory(this)
  // final lazy val AbsHeap = heapFactory(this)
  // final lazy val AbsObj = objFactory(this)
  final lazy val AbsValue = valueFactory(this)
  // final lazy val AbsComp = compFactory(this)
  final lazy val AbsPureValue = pureValueFactory(this)
  final lazy val AbsClo = cloFactory(this)
  final lazy val AbsCont = contFactory(this)
  final lazy val AbsAddr = addrFactory(this)
  final lazy val AbsAstValue = astValueFactory(this)
  final lazy val AbsGrammar = grammarFactory(this)
  final lazy val AbsMath = mathFactory(this)
  final lazy val AbsCodeUnit = codeUnitFactory(this)
  final lazy val AbsConst = constFactory(this)
  final lazy val AbsSimpleValue = simpleValueFactory(this)
  final lazy val AbsNumber = numberFactory(this)
  final lazy val AbsBigInt = bigIntFactory(this)
  final lazy val AbsStr = strFactory(this)
  final lazy val AbsBool = boolFactory(this)
  final lazy val AbsUndef = undefFactory(this)
  final lazy val AbsNull = nullFactory(this)
  final lazy val AbsAbsent = absentFactory(this)
  final lazy val AbsOptAbsent = AbsAbsent.optional(this)

  type AbsState = AbsState.Elem
  // type AbsRet = AbsRet.Elem
  // type AbsHeap = AbsHeap.Elem
  // type AbsObj = AbsObj.Elem
  type AbsValue = AbsValue.Elem
  // type AbsComp = AbsComp.Elem
  type AbsPureValue = AbsPureValue.Elem
  type AbsClo = AbsClo.Elem
  type AbsCont = AbsCont.Elem
  type AbsAddr = AbsAddr.Elem
  type AbsAstValue = AbsAstValue.Elem
  type AbsGrammar = AbsGrammar.Elem
  type AbsMath = AbsMath.Elem
  type AbsCodeUnit = AbsCodeUnit.Elem
  type AbsConst = AbsConst.Elem
  type AbsSimpleValue = AbsSimpleValue.Elem
  type AbsNumber = AbsNumber.Elem
  type AbsBigInt = AbsBigInt.Elem
  type AbsStr = AbsStr.Elem
  type AbsBool = AbsBool.Elem
  type AbsUndef = AbsUndef.Elem
  type AbsNull = AbsNull.Elem
  type AbsAbsent = AbsAbsent.Elem

  // ---------------------------------------------------------------------------
  // abstract domains
  // ---------------------------------------------------------------------------
  final lazy val AbsOptState = AbsState.optional(this)
  // final lazy val AbsOptRet = AbsRet.optional(this)
  // final lazy val AbsOptHeap = AbsHeap.optional(this)
  // final lazy val AbsOptObj = AbsObj.optional(this)
  final lazy val AbsOptValue = AbsValue.optional(this)
  // final lazy val AbsOptComp = AbsComp.optional(this)
  final lazy val AbsOptPureValue = AbsPureValue.optional(this)
  final lazy val AbsOptClo = AbsClo.optional(this)
  final lazy val AbsOptCont = AbsCont.optional(this)
  final lazy val AbsOptAddr = AbsAddr.optional(this)
  final lazy val AbsOptAstValue = AbsAstValue.optional(this)
  final lazy val AbsOptGrammar = AbsGrammar.optional(this)
  final lazy val AbsOptMath = AbsMath.optional(this)
  final lazy val AbsOptCodeUnit = AbsCodeUnit.optional(this)
  final lazy val AbsOptConst = AbsConst.optional(this)
  final lazy val AbsOptSimpleValue = AbsSimpleValue.optional(this)
  final lazy val AbsOptNumber = AbsNumber.optional(this)
  final lazy val AbsOptBigInt = AbsBigInt.optional(this)
  final lazy val AbsOptStr = AbsStr.optional(this)
  final lazy val AbsOptBool = AbsBool.optional(this)
  final lazy val AbsOptUndef = AbsUndef.optional(this)
  final lazy val AbsOptNull = AbsNull.optional(this)

  type AbsOptState = AbsOptState.Elem
  // type AbsOptRet = AbsOptRet.Elem
  // type AbsOptHeap = AbsOptHeap.Elem
  // type AbsOptObj = AbsOptObj.Elem
  type AbsOptValue = AbsOptValue.Elem
  // type AbsOptComp = AbsOptComp.Elem
  type AbsOptPureValue = AbsOptPureValue.Elem
  type AbsOptClo = AbsOptClo.Elem
  type AbsOptCont = AbsOptCont.Elem
  type AbsOptAddr = AbsOptAddr.Elem
  type AbsOptAstValue = AbsOptAstValue.Elem
  type AbsOptGrammar = AbsOptGrammar.Elem
  type AbsOptMath = AbsOptMath.Elem
  type AbsOptCodeUnit = AbsOptCodeUnit.Elem
  type AbsOptConst = AbsOptConst.Elem
  type AbsOptSimpleValue = AbsOptSimpleValue.Elem
  type AbsOptNumber = AbsOptNumber.Elem
  type AbsOptBigInt = AbsOptBigInt.Elem
  type AbsOptStr = AbsOptStr.Elem
  type AbsOptBool = AbsOptBool.Elem
  type AbsOptUndef = AbsOptUndef.Elem
  type AbsOptNull = AbsOptNull.Elem
  type AbsOptAbsent = AbsOptAbsent.Elem
}
