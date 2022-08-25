package esmeta.ai

import esmeta.ai.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.BaseUtils.*

/** mutable analysis configuration */
object Config {
  AbsValue(Str(""))

  /** set control flow graph */
  def setCFG(cfg: CFG): Unit =
    val init = new Initialize(cfg)
    this._cfg = cfg
    this._baseHeap = init.initHeap
    this._base = (for {
      (addr, obj) <- baseHeap.map
      part = Part.from(addr)
      aobj = AbsObj(obj)
    } yield part -> aobj).toMap
    this._baseGlobals = for ((x, v) <- init.initGlobal) yield x -> AbsValue(v)

  /** get control flow graph */
  def cfg: CFG = _cfg
  private var _cfg = CFG()

  /** base concrete heap */
  def baseHeap: Heap = _baseHeap
  private var _baseHeap = Heap()

  /** base abstract heap map */
  def base: Map[Part, AbsObj] = _base
  private var _base: Map[Part, AbsObj] = Map()

  /** base globals */
  def baseGlobals: Map[Id, AbsValue] = _baseGlobals
  private var _baseGlobals: Map[Id, AbsValue] = Map()

  /** loop maximum iteration */
  var LOOP_ITER: Int = 100

  /** loop maximum depth */
  var LOOP_DEPTH: Int = 20

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

  /** infinite sensitivity */
  var INF_SENS: Boolean = true

  /** throw exception for not yet compiled expressions */
  var YET_THROW: Boolean = false

  /** AST sensitivity */
  var ES_CALL_SENS: Int = 20

  /** k-callsite sensitivity for IR functions */
  var IR_CALL_DEPTH: Int = 50

  /** use condition-based refinement */
  var USE_REFINE: Boolean = false
}
