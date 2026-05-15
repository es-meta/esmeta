package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.UnitWalker as LangUnitWalker

import scala.annotation.tailrec

object Analyzer {

  type SymPath = List[String]
  type AbsState = Map[String, SymPath]

  val COPY = "copy"

  /** Abstract Operations that return a copy of their first argument's data
    * structure.
    */
  private val copyAOs: Set[String] =
    Set("IN__SetDataCopy", "IN__MapDataCopy")

  /** Abstract Operations that create a data structure for an internal slot. */
  private val createAOs: Map[String, String] = Map(
    "IN__SetDataCreate" -> "[[SetData]]",
    "IN__MapDataCreate" -> "[[MapData]]",
  )

  private val dataSlots: Set[String] =
    Set("SetData", "WeakSetData", "MapData", "WeakMapData")

  // ---------------------------------------------------------------------------
  // Fresh symbol generation
  // ---------------------------------------------------------------------------

  private var symCounter = 0
  private def freshSym(): String =
    val s = s"x$symCounter"
    symCounter += 1
    s

  // ---------------------------------------------------------------------------
  // Lattice operations (⊔, ⊑)
  // ---------------------------------------------------------------------------

  extension (p1: SymPath)
    /** join (⊔): common suffix with fresh symbol for divergent prefix. Per
      * Refactor.md §Symbolic Path Analysis: π1 ⊔ π2 = (π1' ⊔ π2').f if π1 =
      * π1'.f and π2 = π2'.f π1 ⊔ π2 = (π1' ⊔ π2').copy if π1 = π1'.copy and π2
      * = π2'.copy π1 ⊔ π2 = σ_fresh otherwise
      */
    infix def ⊔(p2: SymPath): SymPath =
      if (p1 == p2) p1 else List(freshSym()) ++ commonSuffix(p1, p2)

    infix def =~=(p2: SymPath): Boolean =
      p1 == p2 || (p1.nonEmpty && p2.nonEmpty && p1.tail == p2.tail)

  private def commonSuffix(p1: SymPath, p2: SymPath): SymPath =
    p1.reverseIterator
      .zip(p2.reverseIterator)
      .takeWhile { case (a, b) => a == b }
      .map(_._1)
      .toList
      .reverse

  extension (s1: AbsState)
    /** join (⊔): intersect keys and join paths */
    infix def ⊔(s2: AbsState): AbsState =
      (s1.keySet intersect s2.keySet).iterator.map { k =>
        k -> (s1(k) ⊔ s2(k))
      }.toMap

    /** Suffix-equal: two states are equivalent if they have the same keys and
      * all paths are suffix-equal.
      */
    infix def =~=(s2: AbsState): Boolean =
      s1.keySet == s2.keySet && s1.keySet.forall(k => s1(k) =~= s2(k))

  // ---------------------------------------------------------------------------
  // Entry points
  // ---------------------------------------------------------------------------

  /** Analyze body, returning annotated AST with per-node states. */
  def analyze(body: Step): AStep =
    symCounter = 0
    val (_, astep) = analyzeStep(body, collectSlotHints(body))
    astep

  /** Variables that are eventually installed into collection data internal
    * slots should be recognized as those data structures while matching earlier
    * list operations in the same algorithm.
    */
  private def collectSlotHints(body: Step): AbsState = {
    var hints = Map.empty[String, SymPath]

    def addHint(name: String, slot: String): Unit = {
      val path = List(freshSym(), s"[[$slot]]")
      hints = hints.updatedWith(name) {
        case Some(prev) => Some(prev ⊔ path)
        case None       => Some(path)
      }
    }

    new LangUnitWalker {
      override def walk(step: Step): Unit = step match {
        case SetStep(Access(_, slot, _, _), ReferenceExpression(ref))
            if dataSlots(slot) =>
          ref match {
            case Variable(name, _, _, _) => addHint(name, slot)
            case _                       =>
          }
          super.walk(step)
        case _ => super.walk(step)
      }
    }.walk(body)

    hints
  }

  // ---------------------------------------------------------------------------
  // Step transfer function (returns exit state + annotated node)
  // ---------------------------------------------------------------------------

  private def analyzeStep(step: Step, state: AbsState): (AbsState, AStep) =
    step match
      // variable binding / mutation
      case LetStep(Variable(x, _, _, _), expr) =>
        val exitState = state.updated(x, evalExpr(expr, state))
        (exitState, AStep(step, state, Nil))

      case SetStep(Variable(x, _, _, _), expr) =>
        val exitState = state.updated(x, evalExpr(expr, state))
        (exitState, AStep(step, state, Nil))

      case SetStep(_: Access, _) =>
        (state, AStep(step, state, Nil))

      // sequential composition
      case BlockStep(StepBlock(subSteps)) =>
        val (exitState, achildren) =
          subSteps.foldLeft((state, List.empty[AStep])) {
            case ((s, acc), sub) =>
              val (nextS, achild) = analyzeStep(sub.step, s)
              (nextS, acc :+ achild)
          }
        (exitState, AStep(step, state, achildren))

      // branch — join both arms
      case IfStep(_, thenStep, elseStep, _) =>
        val (thenExit, aThen) = analyzeStep(thenStep, state)
        val (elseExit, aElse) = elseStep match
          case Some(e) =>
            val (ex, ae) = analyzeStep(e, state)
            (ex, Some(ae))
          case None =>
            (state, None)
        val exitState = thenExit ⊔ elseExit
        (exitState, AStep(step, state, List(aThen) ++ aElse.toList))

      // loops — seed loop variable from iterable, then fixpoint
      case ForEachStep(_, Variable(v, _, _, _), iterExpr, _, body) =>
        val initState = state.updated(v, evalExpr(iterExpr, state))
        val (exitState, aBody) = fixpoint(initState, body)
        (exitState, AStep(step, state, List(aBody)))

      case ForEachIntegerStep(Variable(v, _, _, _), _, _, _, _, _, body) =>
        val (exitState, aBody) =
          fixpoint(state.updated(v, List(freshSym())), body)
        (exitState, AStep(step, state, List(aBody)))

      case ForEachOwnPropertyKeyStep(Variable(v, _, _, _), _, _, _, _, body) =>
        val (exitState, aBody) =
          fixpoint(state.updated(v, List(freshSym())), body)
        (exitState, AStep(step, state, List(aBody)))

      case ForEachParseNodeStep(Variable(v, _, _, _), _, body) =>
        val (exitState, aBody) =
          fixpoint(state.updated(v, List(freshSym())), body)
        (exitState, AStep(step, state, List(aBody)))

      case RepeatStep(_, body) =>
        val (exitState, aBody) = fixpoint(state, body)
        (exitState, AStep(step, state, List(aBody)))

      case _ =>
        (state, AStep(step, state, Nil))

  // ---------------------------------------------------------------------------
  // Fixpoint iteration (converges when join produces no change)
  // ---------------------------------------------------------------------------

  @tailrec
  private def fixpoint(
    state: AbsState,
    body: Step,
  ): (AbsState, AStep) =
    val (bodyExit, aBody) = analyzeStep(body, state)
    val next = state ⊔ bodyExit
    if (next =~= state) (state, aBody)
    else fixpoint(next, body)

  // ---------------------------------------------------------------------------
  // Expression transfer function (interprocedural for known ops)
  // ---------------------------------------------------------------------------

  private def evalExpr(expr: Expression, state: AbsState): SymPath = expr match
    // reference → resolve
    case ReferenceExpression(ref) =>
      resolveRef(ref, state)

    // ? expr / ! expr — unwrap abrupt completion check
    case ReturnIfAbruptExpression(inner, _) =>
      evalExpr(inner, state)

    // list copy: a copy of X
    case ListCopyExpression(inner) =>
      evalExpr(inner, state) :+ COPY

    // list concat: join all operand paths
    case ListConcatExpression(exprs) =>
      exprs.map(evalExpr(_, state)).reduce(_ ⊔ _)

    // interprocedural: copy-like abstract operations
    case InvokeAbstractOperationExpression(name, args, _) if copyAOs(name) =>
      args.headOption
        .map {
          case ReferenceExpression(ref) => resolveRef(ref, state) :+ COPY
          case other                    => evalExpr(other, state) :+ COPY
        }
        .getOrElse(List(freshSym()))

    // interprocedural: data-structure constructors
    case InvokeAbstractOperationExpression(name, _, _)
        if createAOs.contains(name) =>
      List(freshSym(), createAOs(name))

    // unknown expressions get fresh symbols (not Nil)
    case _ => List(freshSym())

  // ---------------------------------------------------------------------------
  // Reference resolution
  // ---------------------------------------------------------------------------

  private def resolveRef(ref: Reference, state: AbsState): SymPath = ref match
    case Variable(name, _, _, _)  => state.getOrElse(name, List(freshSym()))
    case Access(base, slot, _, _) => resolveRef(base, state) :+ s"[[$slot]]"
    case IndexLookup(base, _)     => List(freshSym())
    case ValueOf(base)            => resolveRef(base, state)
    case _                        => List(freshSym())

  /** Resolve a reference to its symbolic path (public, for Unifier use). */
  def resolvePath(ref: Reference, state: AbsState): SymPath =
    resolveRef(ref, state)
}
