package esmeta.solver

import esmeta.ESMetaTest
import esmeta.cfg.*
import esmeta.es.util.Coverage.Cond
import esmeta.es.util.JsonProtocol
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.{ConcurrentPolicy => CP, ManualInfo, ProgressBar}
import esmeta.util.SystemUtils.*
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{ListBuffer, Map => MMap}

/** type edit distance of covered branch sides, by replaying their witnesses
  *
  * sbt "Test/runMain esmeta.solver.EditDistance RUN/branch-coverage.json ..."
  */
object EditDistance {
  given cfg: CFG = ESMetaTest.cfg
  lazy val analyzer: SymAnalyzer = { val an = SymAnalyzer(cfg); an.analyze; an }

  val runLimit = 2
  val replayLimit = 10

  def main(args: Array[String]): Unit = args.foreach(measureRun)

  def measureRun(coverage: String): Unit = {
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given
    val dir = java.io.File(coverage).getParent
    def script(name: String): String =
      val direct = s"$dir/$name"
      readFile(if (exists(direct)) direct else s"$dir/minimal/$name")
    given io.circe.Decoder[(Cond, String)] = c =>
      for {
        cond <- c.downField("condView").downField("cond").as[Cond]
        script <- c.downField("script").as[String]
      } yield cond -> script
    val sides = readJson[List[(Cond, String)]](coverage)
      .groupMap(_._1)(_._2)
      .toList
      .sortBy((c, _) => (c.branch.id, if (c.cond) 0 else 1))
    val results = ConcurrentHashMap[Cond, (String, Measured)]()
    val bar = ProgressBar(
      msg = s"measuring $coverage",
      iterable = sides,
      concurrent = CP.Fixed(Runtime.getRuntime.availableProcessors),
    )
    bar.foreach { (cond, names) =>
      val tried = names.iterator.map(n => n -> measure(cond, script(n)))
      val first = tried.next()
      val picked =
        if (first._2.isRight) first
        else tried.find(_._2.isRight).getOrElse(first)
      results.put(cond, picked)
    }
    val header = Vector(
      "branch",
      "side",
      "edits",
      "type",
      "slot",
      "prop",
      "dropped",
      "status",
      "entry",
      "script",
    )
    val rows = sides.map { (cond, _) =>
      val (name, result) = results.get(cond)
      val measured = result match
        case Right((f, e, d)) =>
          Vector(e.total, e.ty, e.slot, e.prop, d, "ok", f.name)
        case Left(reason) => Vector(-1, -1, -1, -1, -1, reason, "-")
      (Vector(cond.branch.id, cond.cond) ++ measured :+ name)
    }
    dumpFile(
      name = "type edit distance",
      data = (header +: rows).map(_.mkString("\t")).mkString("", "\n", "\n"),
      filename = s"$dir/edit-distance.tsv",
    )
    val replayed = sides.count((c, _) => results.get(c)._2.isRight)
    println(
      f"Replayed: $replayed/${sides.size} (${replayed * 100.0 / sides.size}%.1f%%)",
    )
  }

  type Measured = Either[String, (Func, Edits, Int)]

  private def measure(cond: Cond, js: String): Measured =
    val func = cfg.funcOf(cond.branch)
    // symbolic execution of a builtin starts after its prefix
    val inPrefix =
      func.builtinEntry.exists(func.reachingTo(_).contains(cond.branch))
    if (inPrefix) Left("prefix")
    else
      try {
        val rec = Recorder(cfg.init.from(js), cond)
        val reached =
          try { rec.result; false }
          catch { case Recorder.Hit => true }
        if (!reached) Left("unreached")
        else if (rec.resumed) Left("resumed")
        else
          rec.trace.toRight("chain").flatMap { (f, trace) =>
            val replay = Replay(f, cond, trace)
            replay.edits.map((e, d) => (f, e, d)).toRight("replay")
          }
      } catch {
        case e: Throwable => Left(s"error:${e.getClass.getSimpleName}")
      }

  /** branch decisions per frame, and the calls entering the next frame */
  case class Trace(frames: Vector[Vector[(Int, Boolean)]], calls: Vector[Int])

  /** concrete run recording its branch decisions until the target side */
  class Recorder(initSt: State, cond: Cond)
    extends Interpreter(initSt, timeLimit = Some(runLimit)) {
    private val decided =
      java.util.IdentityHashMap[Context, ListBuffer[(Int, Boolean)]]()
    private val sites = java.util.IdentityHashMap[Context, Call]()
    var trace: Option[(Func, Trace)] = None
    var resumed: Boolean = false

    private def decisions(c: Context): ListBuffer[(Int, Boolean)] =
      decided.computeIfAbsent(c, _ => ListBuffer())

    override protected def createContext(
      call: Call,
      func: Func,
      locals: MMap[Local, Value],
      prevCtxt: Option[Context],
    ): Context =
      val ctxt = super.createContext(call, func, locals, prevCtxt)
      sites.put(ctxt, call)
      ctxt

    override def eval(node: Node): Unit =
      if (st.context.func.builtinEntry == Some(node))
        decisions(st.context).clear()
      super.eval(node)

    override def moveBranch(branch: Branch, b: Boolean): Unit =
      decisions(st.context) += branch.id -> b
      if (branch == cond.branch && b == cond.cond)
        st.context.featureStack match
          case (fs @ BuiltinFeature(f, _) :: _) =>
            val frames = (st.context :: st.callStack
              .map(_.context)
              .takeWhile(_.featureStack eq fs)).reverse
            val calls = frames.tail.map(sites.get(_))
            if (calls.contains(null)) resumed = true
            else if (frames.head.func == f)
              trace = Some(
                f -> Trace(
                  frames.map(decisions(_).toVector).toVector,
                  calls.map(_.id).toVector,
                ),
              )
            throw Recorder.Hit
          case _ =>
      super.moveBranch(branch, b)
  }
  object Recorder {
    object Hit extends scala.util.control.ControlThrowable
  }

  /** symbolic execution keeping only the recorded path */
  class Replay(f: Func, cond: Cond, trace: Trace)
    extends SymInterpreter(
      analyzer,
      f,
      cond.branch,
      side = Some(cond.cond),
      timeLimit = Some(replayLimit),
    ) {
    import this.analyzer.*, SymTy.*

    private val decisions = trace.frames.flatten
    var dropped: Int = 0

    private def taken(c: Config): List[(Int, Boolean)] =
      c.conds.reverse.map(c => c.branch.id -> c.cond)

    private def feasible(s: AbsState): Boolean =
      s.reachable && s.symEnv.forall((_, ty) => !ty.isBottom)

    override def push(config: Config): Unit =
      val onPath = decisions.startsWith(taken(config)) &&
        trace.calls.startsWith(config.calls.reverse.map(_.id))
      val decided = config.conds.size == conds.size + 1
      val unbounded = config.copy(loops = Set.empty, funcs = Set.empty)
      if (!onPath) ()
      else if (decided && !feasible(config.state)) {
        dropped += 1
        super.push(unbounded.copy(state = st))
      } else super.push(unbounded)

    def edits: Option[(Edits, Int)] =
      Iterator
        .continually(nextCandidate)
        .takeWhile(_.isDefined)
        .flatten
        .find { c =>
          taken(c).size == decisions.size - 1 &&
          c.calls.size == trace.calls.size
        }
        .map(c => distance(c.state) -> dropped)

    private def initial(sym: Sym): ValueTy =
      if (sym == SThis.sym) ESValueT
      else if (sym == SArgs.sym) ListT(ESValueT)
      else if (sym == SNewTarget.sym) ConstructorT || UndefT
      else ESValueT

    private def distance(s: AbsState): Edits =
      s.symEnv.keysIterator.foldLeft(Edits()) { (acc, sym) =>
        acc + EditDistance.edits(initial(sym), s.getConstr(sym))
      }
  }

  enum Level { case Type, Prop, Slot }

  case class Edits(ty: Int = 0, prop: Int = 0, slot: Int = 0) {
    def +(that: Edits): Edits =
      Edits(ty + that.ty, prop + that.prop, slot + that.slot)
    def total: Int = ty + prop + slot
  }
  object Edits {
    def one(level: Level): Edits = level match
      case Level.Type => Edits(ty = 1)
      case Level.Prop => Edits(prop = 1)
      case Level.Slot => Edits(slot = 1)
  }

  /** edit distance between two input types */
  def edits(from: ValueTy, to: ValueTy): Edits =
    if (same(from, to)) Edits()
    else
      val narrowed =
        if (same(typeLevel(from), typeLevel(to))) Edits()
        else Edits.one(Level.Type)
      narrowed + fieldEdits(to, Level.Type)

  private def same(l: ValueTy, r: ValueTy): Boolean = l <= r && r <= l

  private def typeLevel(ty: ValueTy): ValueTy = ty match
    case e: ValueElemTy =>
      e.record match
        case RecordTy.Elem(map, _) =>
          e.copy(record = RecordTy.Elem(map.map((t, _) => t -> FieldMap.Top)))
        case _ => e
    case _ => ty

  private def fieldEdits(ty: ValueTy, level: Level): Edits = ty match
    case e: ValueElemTy if e.copy(record = RecordTy.Bot).isBottom =>
      e.record match
        case RecordTy.Elem(map, obj) =>
          val slots = map.map(slotEdits).minByOption(_.total)
          slots.getOrElse(Edits()) + shapeEdits(obj, level)
        case _ => Edits()
    case _ => Edits()

  private def slotEdits(tname: String, fm: FieldMap): Edits =
    fm.map.foldLeft(Edits()) {
      case (acc, (f, b)) =>
        val declared = ManualInfo.tyModel.getField((tname, f))
        if (declared.isAbsent || !(declared.value <= b.value))
          acc + Edits.one(Level.Slot) + fieldEdits(b.value, Level.Slot)
        else acc
    }

  private def shapeEdits(obj: ObjShape, level: Level): Edits =
    val inner = if (level == Level.Slot) Level.Slot else Level.Prop
    val props =
      obj.props.values.filter(_ != Desc.Top).foldLeft(Edits()) { (acc, d) =>
        acc + Edits.one(inner) + fieldEdits(d.ty, inner)
      }
    val call = if (obj.call == CallDesc.Top) Edits() else Edits.one(inner)
    val construct =
      if (obj.construct == ConstructDesc.Top) Edits() else Edits.one(inner)
    props + call + construct
}
