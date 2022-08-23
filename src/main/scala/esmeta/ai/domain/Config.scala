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
  cloFactory: CloFactory = _ => new CloDomain with FlatDomain[Clo]("clo"),
  contFactory: ContFactory = _ => new ContDomain with FlatDomain[Cont]("cont"),
  addrFactory: AddrFactory = _ => addr.Domain(addr.AllocSitePartition),
  astValueFactory: AstValueFactory = _ =>
    new AstValueDomain with FlatDomain[AstValue]("astValue"),
  grammarFactory: GrammarFactory = _ =>
    new GrammarDomain with FlatDomain[Grammar]("grammar"),
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
  final lazy val AbsState: StateDomain = stateFactory(this)
  final lazy val AbsValue: ValueDomain = valueFactory(this)
  // final lazy val AbsRet: RetDomain = retFactory(this)
  // final lazy val AbsHeap: HeapDomain = heapFactory(this)
  // final lazy val AbsObj: ObjDomain = objFactory(this)
  // final lazy val AbsComp: CompDomain = compFactory(this)
  final lazy val AbsClo: CloDomain = cloFactory(this)
  final lazy val AbsCont: ContDomain = contFactory(this)
  final lazy val AbsAddr: AddrDomain = addrFactory(this)
  final lazy val AbsAstValue: AstValueDomain = astValueFactory(this)
  final lazy val AbsGrammar: GrammarDomain = grammarFactory(this)
  final lazy val AbsCodeUnit: CodeUnitDomain = codeUnitFactory(this)
  final lazy val AbsConst: ConstDomain = constFactory(this)
  final lazy val AbsSimpleValue: SimpleValueDomain = simpleValueFactory(this)
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
  type AbsClo = AbsClo.Elem
  type AbsCont = AbsCont.Elem
  type AbsAddr = AbsAddr.Elem
  type AbsAstValue = AbsAstValue.Elem
  type AbsGrammar = AbsGrammar.Elem
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
}
