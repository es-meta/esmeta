package esmeta.analyzer

import esmeta.analyzer.util.*
import esmeta.cfg.{util => _, *}
import esmeta.state.{util => _, *}
import esmeta.util.BaseUtils.*

/** static analyzer */
abstract class Analyzer
  extends AbsTransferDecl
  with AbsSemanticsDecl
  with AnalysisPointDecl
  with TypeMismatchDecl
  with ViewDecl
  with domain.Decl
  with repl.Decl
  with util.Decl {

  /** control flow graph */
  val cfg: CFG

  /** abstract semantics */
  type Semantics <: AbsSemantics
  lazy val sem: Semantics

  /** transfer function */
  type Transfer <: AbsTransfer
  lazy val transfer: Transfer

  /** analyzer elements */
  trait AnalyzerElem {
    override def toString: String = toString(true, false, false)

    /** stringify with options */
    def toString(
      detail: Boolean = false,
      line: Boolean = false,
      asite: Boolean = false,
    ): String =
      val stringifier = AnalyzerElem.getStringifier(detail, line, asite)
      import stringifier.elemRule
      stringify(this)
  }
  object AnalyzerElem {
    val getStringifier = cached[(Boolean, Boolean, Boolean), Stringifier] {
      Stringifier(_, _, _)
    }
  }

  // ---------------------------------------------------------------------------
  // analysis options
  // ---------------------------------------------------------------------------
  /** analysis time limit */
  val timeLimit: Option[Long] = None

  /** debugging mode */
  val debug: Boolean = false

  /** REPL mode */
  val useRepl: Boolean = false

  /** Run continue command at startup when using repl */
  val replContinue: Boolean = false

  /** check period */
  val checkPeriod: Int = 10000

  /** IR sensitivity */
  val irSens: Boolean = true

  /** throw exception for not yet compiled expressions */
  val yetThrow: Boolean = false

  /** use condition-based refinement */
  val useRefine: Boolean = false

  // ---------------------------------------------------------------------------
  // shortcuts
  // ---------------------------------------------------------------------------
  lazy val AB = AbsBool.Top
  lazy val AT = AbsBool(T)
  lazy val AF = AbsBool(F)
  lazy val AVT = AbsValue(T)
  lazy val AVF = AbsValue(F)
  lazy val AVB = AbsValue(T, F)
  lazy val AV_TYPE = AbsValue(Str("Type"))
  lazy val AV_VALUE = AbsValue(Str("Value"))
  lazy val AV_TARGET = AbsValue(Str("Target"))

  // ---------------------------------------------------------------------------
  // abstract domains
  // ---------------------------------------------------------------------------
  protected var stateDomain: Option[StateDomain] = None
  protected var retDomain: Option[RetDomain] = None
  protected var heapDomain: Option[HeapDomain] = None
  protected var objDomain: Option[ObjDomain] = None
  protected var valueDomain: Option[ValueDomain] = None
  protected var compDomain: Option[CompDomain] = None
  protected var pureValueDomain: Option[PureValueDomain] = None
  protected var cloDomain: Option[CloDomain] = None
  protected var contDomain: Option[ContDomain] = None
  protected var partDomain: Option[PartDomain] = None
  protected var astValueDomain: Option[AstValueDomain] = None
  protected var ntDomain: Option[NtDomain] = None
  protected var mathDomain: Option[MathDomain] = None
  protected var infinityDomain: Option[InfinityDomain] = None
  protected var codeUnitDomain: Option[CodeUnitDomain] = None
  protected var constDomain: Option[ConstDomain] = None
  protected var simpleValueDomain: Option[SimpleValueDomain] = None
  protected var numberDomain: Option[NumberDomain] = None
  protected var bigIntDomain: Option[BigIntDomain] = None
  protected var strDomain: Option[StrDomain] = None
  protected var boolDomain: Option[BoolDomain] = None
  protected var undefDomain: Option[UndefDomain] = None
  protected var nullDomain: Option[NullDomain] = None
  protected var absentDomain: Option[AbsentDomain] = None

  final lazy val AbsState = stateDomain.getOrElse(StateBasicDomain)
  type AbsState = AbsState.Elem

  final lazy val AbsRet = retDomain.getOrElse(RetBasicDomain)
  type AbsRet = AbsRet.Elem

  final lazy val AbsHeap = heapDomain.getOrElse(HeapBasicDomain)
  type AbsHeap = AbsHeap.Elem

  final lazy val AbsObj = objDomain.getOrElse(ObjBasicDomain)
  type AbsObj = AbsObj.Elem

  final lazy val AbsValue = valueDomain.getOrElse(ValueBasicDomain)
  type AbsValue = AbsValue.Elem

  final lazy val AbsComp = compDomain.getOrElse(CompBasicDomain)
  type AbsComp = AbsComp.Elem

  final lazy val AbsPureValue = pureValueDomain.getOrElse(PureValueBasicDomain)
  type AbsPureValue = AbsPureValue.Elem

  final lazy val AbsClo = cloDomain.getOrElse(CloSetDomain())
  type AbsClo = AbsClo.Elem

  final lazy val AbsCont = contDomain.getOrElse(ContSetDomain())
  type AbsCont = AbsCont.Elem

  final lazy val AbsPart = partDomain.getOrElse(PartSetDomain())
  type AbsPart = AbsPart.Elem

  final lazy val AbsAstValue = astValueDomain.getOrElse(AstValueFlatDomain)
  type AbsAstValue = AbsAstValue.Elem

  final lazy val AbsNt = ntDomain.getOrElse(NtFlatDomain)
  type AbsNt = AbsNt.Elem

  final lazy val AbsMath = mathDomain.getOrElse(MathFlatDomain)
  type AbsMath = AbsMath.Elem

  final lazy val AbsInfinity = infinityDomain.getOrElse(InfinityFlatDomain)
  type AbsInfinity = AbsInfinity.Elem

  final lazy val AbsCodeUnit = codeUnitDomain.getOrElse(CodeUnitFlatDomain)
  type AbsCodeUnit = AbsCodeUnit.Elem

  final lazy val AbsConst = constDomain.getOrElse(ConstFlatDomain)
  type AbsConst = AbsConst.Elem

  final lazy val AbsSimpleValue =
    simpleValueDomain.getOrElse(SimpleValueBasicDomain)
  type AbsSimpleValue = AbsSimpleValue.Elem

  final lazy val AbsNumber = numberDomain.getOrElse(NumberFlatDomain)
  type AbsNumber = AbsNumber.Elem

  final lazy val AbsBigInt = bigIntDomain.getOrElse(BigIntFlatDomain)
  type AbsBigInt = AbsBigInt.Elem

  final lazy val AbsStr = strDomain.getOrElse(StrSetDomain())
  type AbsStr = AbsStr.Elem

  final lazy val AbsBool = boolDomain.getOrElse(BoolFlatDomain)
  type AbsBool = AbsBool.Elem

  final lazy val AbsUndef = undefDomain.getOrElse(UndefSimpleDomain)
  type AbsUndef = AbsUndef.Elem

  final lazy val AbsNull = nullDomain.getOrElse(NullSimpleDomain)
  type AbsNull = AbsNull.Elem

  final lazy val AbsAbsent = absentDomain.getOrElse(AbsentSimpleDomain)
  type AbsAbsent = AbsAbsent.Elem
}
