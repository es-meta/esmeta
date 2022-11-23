package esmeta.analyzer

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.Param
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

/** specification type analyzer for ECMA-262 */
private class TypeAnalyzer(
  cfg: CFG,
  targets: List[Func],
  ignorePath: Option[String],
  ignoreSet: Set[String],
  log: Boolean = false,
) {

  // initilize analysis with CFG and logging path
  lazy val result: TypeSemantics = withCFG(cfg) {
    withSem(sem) {
      sem.fixpoint
      if (log) logging
      var unusedSet = ignoreSet
      val mismatches = sem.getMismatches.filter {
        case mismatch =>
          val name = mismatch.name
          unusedSet -= name
          !ignoreSet.contains(name)
      }
      if (!mismatches.isEmpty) {
        // warning message
        warn(
          s"${mismatches.size} type mismatches are detected" + LINE_SEP +
          mismatches.toList.map(_.toString).sorted.mkString(LINE_SEP) +
          // show help message about how to use the ignorance system
          ignorePath.fold("")(path =>
            LINE_SEP + "=" * 80 +
            LINE_SEP + s"Please add the following callee names to `$path`:" +
            mismatches
              .map(LINE_SEP + "  - " + _.name)
              .toList
              .sorted
              .mkString +
            LINE_SEP + "=" * 80,
          ),
        )
        throw TypeMismatchError(mismatches)
      }
      if (!unusedSet.isEmpty) throw UnnecessaryIgnore(unusedSet)
      sem
    }
  }

  // type semantics
  lazy val sem: TypeSemantics = TypeSemantics(initNpMap)

  // all entry node points
  lazy val nps: List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  // get initial abstract states in each node point
  lazy val initNpMap: Map[NodePoint[Node], AbsState] = (for {
    np @ NodePoint(func, _, _) <- nps
    st = getState(func)
  } yield np -> st).toMap

  // get view from a function
  def getView(func: Func): View = View()

  // get return point
  def getState(func: Func): AbsState = func.params.foldLeft(AbsState.Empty) {
    case (st, Param(x, ty, opt, _)) =>
      var v = AbsValue(ty.ty)
      if (opt) v ⊔= AbsValue.absentTop
      st.update(x, v)
  }

  // logging mode
  def logging: Unit = {
    mkdir(ANALYZE_LOG_DIR)
    dumpFile(
      name = "type analysis result",
      data = sem.typesString,
      filename = s"$ANALYZE_LOG_DIR/types",
    )
    dumpFile(
      name = "visiting counter for control points",
      data = sem.getCounter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/counter",
    )
    dumpFile(
      name = "detected type mismatches",
      data = sem.getMismatches.toList
        .map(_.toString)
        .sorted
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/mismatches",
    )
  }
}
object TypeAnalyzer:
  // set type domains
  initDomain(
    stateDomain = state.TypeDomain,
    valueDomain = value.TypeDomain,
    retDomain = ret.TypeDomain,
  )

  // no sensitivity
  IR_SENS = false

  // use type refinement
  USE_REFINE = true

  /** perform type analysis for given target functions */
  def apply(
    cfg: CFG,
    targets: List[Func],
    ignorePath: Option[String],
    ignoreSet: Set[String],
    log: Boolean = false,
  ): TypeSemantics =
    new TypeAnalyzer(cfg, targets, ignorePath, ignoreSet, log).result
