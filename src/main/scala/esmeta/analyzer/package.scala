package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.es.Initialize
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.BaseUtils.*

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

/** exploded */
def exploded(msg: String): Nothing = throw AnalysisImprecise(msg)

/** not supported */
def notSupported(msg: String): Nothing = throw NotSupported(msg)

/** initilize control flow graphs (multiple time allowed) */
def initCFG(cfg: CFG): Unit =
  this._cfg = cfg
  AbsState.setBase(new Initialize(cfg))

// -----------------------------------------------------------------------------
// mutable global options
// -----------------------------------------------------------------------------
/** get control flow graph */
def cfg: CFG = _cfg
private var _cfg = CFG()

/** analysis time limit */
var TIME_LIMIT: Option[Long] = None

/** debugging mode */
var DEBUG: Boolean = false

/** REPL mode */
var USE_REPL: Boolean = false

/** check period */
var CHECK_PERIOD: Int = 10000

/** IR sensitivity */
var IR_SENS: Boolean = true

/** throw exception for not yet compiled expressions */
var YET_THROW: Boolean = false

/** use condition-based refinement */
var USE_REFINE: Boolean = false

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
/** initilize analysis domains (only once) */
def initDomain(
  stateDomain: StateDomain = state.BasicDomain,
  retDomain: RetDomain = ret.BasicDomain,
  heapDomain: HeapDomain = heap.BasicDomain,
  objDomain: ObjDomain = obj.BasicDomain,
  valueDomain: ValueDomain = value.BasicDomain,
  compDomain: CompDomain = comp.BasicDomain,
  pureValueDomain: PureValueDomain = pureValue.BasicDomain,
  cloDomain: CloDomain = clo.SetDomain(),
  contDomain: ContDomain = cont.SetDomain(),
  partDomain: PartDomain = part.SetDomain(),
  astValueDomain: AstValueDomain = astValue.FlatDomain,
  grammarDomain: GrammarDomain = grammar.FlatDomain,
  mathDomain: MathDomain = math.FlatDomain,
  codeUnitDomain: CodeUnitDomain = codeUnit.FlatDomain,
  constDomain: ConstDomain = const.FlatDomain,
  simpleValueDomain: SimpleValueDomain = simpleValue.BasicDomain,
  numberDomain: NumberDomain = number.FlatDomain,
  bigIntDomain: BigIntDomain = bigInt.FlatDomain,
  strDomain: StrDomain = str.SetDomain(),
  boolDomain: BoolDomain = bool.FlatDomain,
  undefDomain: UndefDomain = undef.SimpleDomain,
  nullDomain: NullDomain = nullv.SimpleDomain,
  absentDomain: AbsentDomain = absent.SimpleDomain,
): Unit =
  if (initialized) error("analysis configuration is already initialized")
  _stateDomain = Some(stateDomain)
  _retDomain = Some(retDomain)
  _heapDomain = Some(heapDomain)
  _objDomain = Some(objDomain)
  _valueDomain = Some(valueDomain)
  _compDomain = Some(compDomain)
  _pureValueDomain = Some(pureValueDomain)
  _cloDomain = Some(cloDomain)
  _contDomain = Some(contDomain)
  _partDomain = Some(partDomain)
  _astValueDomain = Some(astValueDomain)
  _grammarDomain = Some(grammarDomain)
  _mathDomain = Some(mathDomain)
  _codeUnitDomain = Some(codeUnitDomain)
  _constDomain = Some(constDomain)
  _simpleValueDomain = Some(simpleValueDomain)
  _numberDomain = Some(numberDomain)
  _bigIntDomain = Some(bigIntDomain)
  _strDomain = Some(strDomain)
  _boolDomain = Some(boolDomain)
  _undefDomain = Some(undefDomain)
  _nullDomain = Some(nullDomain)
  _absentDomain = Some(absentDomain)

// domain initialized
private var initialized = false

// private domains
private var _stateDomain: Option[StateDomain] = None
private var _retDomain: Option[RetDomain] = None
private var _heapDomain: Option[HeapDomain] = None
private var _objDomain: Option[ObjDomain] = None
private var _valueDomain: Option[ValueDomain] = None
private var _compDomain: Option[CompDomain] = None
private var _pureValueDomain: Option[PureValueDomain] = None
private var _cloDomain: Option[CloDomain] = None
private var _contDomain: Option[ContDomain] = None
private var _partDomain: Option[PartDomain] = None
private var _astValueDomain: Option[AstValueDomain] = None
private var _grammarDomain: Option[GrammarDomain] = None
private var _mathDomain: Option[MathDomain] = None
private var _codeUnitDomain: Option[CodeUnitDomain] = None
private var _constDomain: Option[ConstDomain] = None
private var _simpleValueDomain: Option[SimpleValueDomain] = None
private var _numberDomain: Option[NumberDomain] = None
private var _bigIntDomain: Option[BigIntDomain] = None
private var _strDomain: Option[StrDomain] = None
private var _boolDomain: Option[BoolDomain] = None
private var _undefDomain: Option[UndefDomain] = None
private var _nullDomain: Option[NullDomain] = None
private var _absentDomain: Option[AbsentDomain] = None

type StateDomain = state.Domain
lazy val AbsState = _stateDomain.getOrElse(state.BasicDomain)
type AbsState = AbsState.Elem

type RetDomain = ret.Domain
lazy val AbsRet = _retDomain.getOrElse(ret.BasicDomain)
type AbsRet = AbsRet.Elem

type HeapDomain = heap.Domain
lazy val AbsHeap = _heapDomain.getOrElse(heap.BasicDomain)
type AbsHeap = AbsHeap.Elem

type ObjDomain = obj.Domain
lazy val AbsObj = _objDomain.getOrElse(obj.BasicDomain)
type AbsObj = AbsObj.Elem

type ValueDomain = value.Domain
lazy val AbsValue = _valueDomain.getOrElse(value.BasicDomain)
type AbsValue = AbsValue.Elem

type CompDomain = comp.Domain
lazy val AbsComp = _compDomain.getOrElse(comp.BasicDomain)
type AbsComp = AbsComp.Elem

type PureValueDomain = pureValue.Domain
lazy val AbsPureValue = _pureValueDomain.getOrElse(pureValue.BasicDomain)
type AbsPureValue = AbsPureValue.Elem

type CloDomain = clo.Domain
lazy val AbsClo = _cloDomain.getOrElse(clo.SetDomain())
type AbsClo = AbsClo.Elem

type ContDomain = cont.Domain
lazy val AbsCont = _contDomain.getOrElse(cont.SetDomain())
type AbsCont = AbsCont.Elem

type PartDomain = part.Domain
lazy val AbsPart = _partDomain.getOrElse(part.SetDomain())
type AbsPart = AbsPart.Elem

type AstValueDomain = astValue.Domain
lazy val AbsAstValue = _astValueDomain.getOrElse(astValue.FlatDomain)
type AbsAstValue = AbsAstValue.Elem

type GrammarDomain = grammar.Domain
lazy val AbsGrammar = _grammarDomain.getOrElse(grammar.FlatDomain)
type AbsGrammar = AbsGrammar.Elem

type MathDomain = math.Domain
lazy val AbsMath = _mathDomain.getOrElse(math.FlatDomain)
type AbsMath = AbsMath.Elem

type CodeUnitDomain = codeUnit.Domain
lazy val AbsCodeUnit = _codeUnitDomain.getOrElse(codeUnit.FlatDomain)
type AbsCodeUnit = AbsCodeUnit.Elem

type ConstDomain = const.Domain
lazy val AbsConst = _constDomain.getOrElse(const.FlatDomain)
type AbsConst = AbsConst.Elem

type SimpleValueDomain = simpleValue.Domain
lazy val AbsSimpleValue = _simpleValueDomain.getOrElse(simpleValue.BasicDomain)
type AbsSimpleValue = AbsSimpleValue.Elem

type NumberDomain = number.Domain
lazy val AbsNumber = _numberDomain.getOrElse(number.FlatDomain)
type AbsNumber = AbsNumber.Elem

type BigIntDomain = bigInt.Domain
lazy val AbsBigInt = _bigIntDomain.getOrElse(bigInt.FlatDomain)
type AbsBigInt = AbsBigInt.Elem

type StrDomain = str.Domain
lazy val AbsStr = _strDomain.getOrElse(str.SetDomain())
type AbsStr = AbsStr.Elem

type BoolDomain = bool.Domain
lazy val AbsBool = _boolDomain.getOrElse(bool.FlatDomain)
type AbsBool = AbsBool.Elem

type UndefDomain = undef.Domain
lazy val AbsUndef = _undefDomain.getOrElse(undef.SimpleDomain)
type AbsUndef = AbsUndef.Elem

type NullDomain = nullv.Domain
lazy val AbsNull = _nullDomain.getOrElse(nullv.SimpleDomain)
type AbsNull = AbsNull.Elem

type AbsentDomain = absent.Domain
lazy val AbsAbsent = _absentDomain.getOrElse(absent.SimpleDomain)
type AbsAbsent = AbsAbsent.Elem
