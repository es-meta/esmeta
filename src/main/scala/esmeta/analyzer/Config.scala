package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** mutable analysis configuration */
object Config {

  /** set control flow graph */
  def setCFG(cfg: CFG): Unit =
    this._cfg = cfg
    logger.reset
    AbsState.setBase(new Initialize(cfg))

  lazy val logger: Logger = new Logger

  /** get control flow graph */
  def cfg: CFG = _cfg
  private var _cfg = CFG()

  /** base abstract heap map */
  def baseHeap: Map[Part, AbsObj] = _baseHeap
  private var _baseHeap: Map[Part, AbsObj] = Map()

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
