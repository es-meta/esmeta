package esmeta.ai.domain

import esmeta.cfg.*
import esmeta.es.*
import esmeta.state.*
import esmeta.util.BaseUtils.*

/** mutable analysis configuration */
object Config {
  // ---------------------------------------------------------------------------
  // control flow graphs
  // ---------------------------------------------------------------------------
  def setCFG(cfg: CFG): Unit =
    this._cfg = cfg
    this._baseHeap = new Initialize(cfg).initHeap
    this._base = (for {
      (addr, obj) <- baseHeap.map
      part = Part.from(addr)
      aobj = AbsObj(obj)
    } yield part -> aobj).toMap
  def cfg: CFG = _cfg
  def baseHeap: Heap = _baseHeap
  def base: Map[Part, AbsObj] = _base
  private var _cfg = CFG()
  private var _baseHeap = Heap()
  private var _base: Map[Part, AbsObj] = Map()
  // ---------------------------------------------------------------------------
  // abstract domains
  // ---------------------------------------------------------------------------
  def setDomain(
    stateDomain: StateDomain = state.BasicDomain,
    // retDomain: RetDomain,
    heapDomain: HeapDomain = heap.BasicDomain,
    objDomain: ObjDomain = obj.BasicDomain,
    valueDomain: ValueDomain = value.BasicDomain,
    compDomain: CompDomain = comp.BasicDomain,
    pureValueDomain: PureValueDomain = pureValue.BasicDomain,
    cloDomain: CloDomain = clo.FlatDomain,
    contDomain: ContDomain = cont.FlatDomain,
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
    if (initialized) error("already initialized")
    this._stateDomain = stateDomain
    // this._retDomain = retDomain
    this._heapDomain = heapDomain
    this._objDomain = objDomain
    this._valueDomain = valueDomain
    this._compDomain = compDomain
    this._pureValueDomain = pureValueDomain
    this._cloDomain = cloDomain
    this._contDomain = contDomain
    this._partDomain = partDomain
    this._astValueDomain = astValueDomain
    this._grammarDomain = grammarDomain
    this._mathDomain = mathDomain
    this._codeUnitDomain = codeUnitDomain
    this._constDomain = constDomain
    this._simpleValueDomain = simpleValueDomain
    this._numberDomain = numberDomain
    this._bigIntDomain = bigIntDomain
    this._strDomain = strDomain
    this._boolDomain = boolDomain
    this._undefDomain = undefDomain
    this._nullDomain = nullDomain
    this._absentDomain = absentDomain
  private var _stateDomain: StateDomain = state.BasicDomain
  // private var _retDomain: RetDomain
  private var _heapDomain: HeapDomain = heap.BasicDomain
  private var _objDomain: ObjDomain = obj.BasicDomain
  private var _valueDomain: ValueDomain = value.BasicDomain
  private var _compDomain: CompDomain = comp.BasicDomain
  private var _pureValueDomain: PureValueDomain = pureValue.BasicDomain
  private var _cloDomain: CloDomain = clo.FlatDomain
  private var _contDomain: ContDomain = cont.FlatDomain
  private var _partDomain: PartDomain = part.SetDomain()
  private var _astValueDomain: AstValueDomain = astValue.FlatDomain
  private var _grammarDomain: GrammarDomain = grammar.FlatDomain
  private var _mathDomain: MathDomain = math.FlatDomain
  private var _codeUnitDomain: CodeUnitDomain = codeUnit.FlatDomain
  private var _constDomain: ConstDomain = const.FlatDomain
  private var _simpleValueDomain: SimpleValueDomain = simpleValue.BasicDomain
  private var _numberDomain: NumberDomain = number.FlatDomain
  private var _bigIntDomain: BigIntDomain = bigInt.FlatDomain
  private var _strDomain: StrDomain = str.SetDomain()
  private var _boolDomain: BoolDomain = bool.FlatDomain
  private var _undefDomain: UndefDomain = undef.SimpleDomain
  private var _nullDomain: NullDomain = nullv.SimpleDomain
  private var _absentDomain: AbsentDomain = absent.SimpleDomain
  // ---------------------------------------------------------------------------
  // initialization
  // ---------------------------------------------------------------------------
  lazy val (
    stateDomain,
    // retDomain,
    heapDomain,
    objDomain,
    valueDomain,
    compDomain,
    pureValueDomain,
    cloDomain,
    contDomain,
    partDomain,
    astValueDomain,
    grammarDomain,
    mathDomain,
    codeUnitDomain,
    constDomain,
    simpleValueDomain,
    numberDomain,
    bigIntDomain,
    strDomain,
    boolDomain,
    undefDomain,
    nullDomain,
    absentDomain,
  ) =
    if (initialized) error("already initialized")
    initialized = true
    (
      _stateDomain,
      // _retDomain,
      _heapDomain,
      _objDomain,
      _valueDomain,
      _compDomain,
      _pureValueDomain,
      _cloDomain,
      _contDomain,
      _partDomain,
      _astValueDomain,
      _grammarDomain,
      _mathDomain,
      _codeUnitDomain,
      _constDomain,
      _simpleValueDomain,
      _numberDomain,
      _bigIntDomain,
      _strDomain,
      _boolDomain,
      _undefDomain,
      _nullDomain,
      _absentDomain,
    )
  private var initialized = false
}
