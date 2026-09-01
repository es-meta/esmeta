package esmeta.solver

import esmeta.cfg.{Block, Branch, Call, CFG, Func}
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.*
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** how a slot's constraint shows up in program text */
class TemplateDeriver(cfg: CFG) {

  import TemplateDeriver.*, Hole.*, Rec.*, Val.*

  // ---------------------------------------------------------------------------
  // templates the spec states
  // ---------------------------------------------------------------------------

  /** the owner decides the shape, the origin decides where the hole goes */
  lazy val fromWrites: List[(String, Template)] = for {
    f <- cfg.funcs
    surface <- Solver.funcAccessExpr(f).toList
    (into, reached) <- writesBy(f)
    if reached.nonEmpty
    slots = reached.values.flatMap(_._1.slot).toSet
    // a constant the spec writes tells which type declares it
    fixed = reached.values
      .collect {
        case (h, Const(t)) => h.slot.map(_ -> t)
      }
      .flatten
      .toMap
    owner <-
      if (into == Rec.Changed) receiverOf(slots, fixed)
      else ownerOf(slots, fixed)
    t <-
      shapes(
        owner,
        reached,
        surface,
        usesNewTarget(f),
        argWidth(f.name),
        into,
      )
  } yield owner -> t

  // every slot the builtin writes, traced back to a place the caller controls
  private def writesBy(f: Func): List[(Rec, Map[String, (Hole, Val)])] =
    val org = holds(f)
    val out = flowsToReturn(f.name)
    val fresh = freshOf(f.name)
    val from = calls(f)
    val built = records(f)
    val read = projected(f, org)
    // the builtin writes it itself
    val direct = body(f.name).flatMap {
      case IAssign(Field(b: Local, EStr(slot)), v) if fresh(b) && out(b) =>
        placed(slot, v, from, org).map(e => (Rec.Created, b) -> e)
      // an append names the receiver, so the caller must hand one over
      case IPush(v, ERef(Field(b: Local, EStr(slot))), _)
          if sources(ERef(b), org).contains(Receiver) =>
        appended(slot, v, org, built).toList.map(e => (Rec.Changed, b) -> e)
      case _ => Nil
    }
    // an operation it calls writes it, on an object that comes back out
    val onward = for {
      case ICall(lhs, EClo(g, _), args) <- body(f.name)
      was = writeSummary.getOrElse(g, WriteSummary())
      slots <- (if (out(lhs)) was.created else Nil) ++ (for {
        (there, fills) <- was.changed.toList.sortBy(_._1)
        case ERef(h: Local) <- args.lift(there).toList
        if out(h)
      } yield fills)
      reached = slots.toList.flatMap { (slot, fs) =>
        // a hole the caller can fill beats a constant the spec happened to write
        val ins = fs.collect { case i: Arg => i }
        (if (ins.nonEmpty) ins else fs).toList.flatMap {
          // the callee already fixed it, so no argument of ours carries it
          case c: Val.Const => List(named(Hole.Slot(slot))(c))
          case Arg(i) =>
            val arg = args.lift(i)
            val direct = arg
              .toSet[Expr]
              .flatMap(sources(_, org))
              .map(named(Hole.Slot(slot)))
            if (direct.nonEmpty) direct.toList
            else
              // the hole holds an object carrying the value, not the value
              arg
                .collect { case ERef(l: Local) => l }
                .flatMap(read.get)
                .map { (place, prop) =>
                  named(Hole.Slot(slot, source = Some(prop)))(place)
                }
                .toList
        }
      }.toMap
      if reached.nonEmpty
    } yield (Rec.Created, reached)
    direct.groupMap(_._1)(_._2).toList.map {
      case ((into, _), es) => into -> es.toMap
    } ++ onward

  // the argument itself, or a field of what was built around it
  private def placed(
    slot: String,
    v: Expr,
    from: Map[Local, (String, List[Expr])],
    org: Map[Local, Set[Val]],
  ): List[(String, (Hole, Val))] =
    val direct = sources(v, org).toList.map(named(Hole.Slot(slot)))
    if (direct.nonEmpty) direct.toList
    else
      for {
        case ERef(l: Local) <- List(v)
        (g, args) <- from.get(l).toList
        built <- writeSummary.getOrElse(g, WriteSummary()).created
        (field, fs) <- built.toList.sortBy(_._1)
        case Arg(i) <- fs.toList
        arg <- args.lift(i).toList
        place <- sources(arg, org).toList
      } yield named(Hole.Slot(slot, List(field)))(place)

  // what it appends may be built of several arguments, each its own place
  private def appended(
    slot: String,
    v: Expr,
    org: Map[Local, Set[Val]],
    built: Map[Local, ERecord],
  ): Map[String, (Hole, Val)] =
    v match
      case ERef(l: Local) if built.contains(l) =>
        (for {
          (field, e) <- built(l).pairs
          o <- sources(e, org).toList
          steps = List(elemName, field)
        } yield named(Hole.Slot(slot, steps))(o)).toMap
      case _ =>
        val h = Hole.Slot(slot, List(elemName))
        sources(v, org).map(named(h)).toMap

  // a hole is keyed by the name the surface writes, which the hole itself gives
  private def named(h: Hole): Val => (String, (Hole, Val)) =
    o => h.name -> (h, o)

  private def shapes(
    owner: String,
    reached: Map[String, (Hole, Val)],
    surface: String,
    isCtor: Boolean,
    arity: Int,
    into: Rec,
  ): List[Template] =
    val held = into == Rec.Changed
    // an argument becomes a slot to fill, a constant becomes part of the type
    val fixed = MMap[String, ValueTy]()
    val at = MMap[Int, String]()
    var receiver: Option[String] = None
    for ((name, (h, origin)) <- reached.toList.sortBy(_._1)) origin match
      case Receiver  => receiver = Some(name)
      case Arg(i)    => at(i) = name
      case Const(ty) => h.slot.foreach(fixed(_) = ty)
    val ordered = at.toList.sortBy(_._1)
    val self = if (held) "$" + baseName else receiver.fold("undefined")("$" + _)
    // arguments are positional, so a template that stops short is one too
    for {
      taken <- ordered.inits.toList.reverse
      names = (if (held) List(baseName) else receiver.toList) ++
        taken.map(_._2).filterNot(receiver.contains)
      if names.nonEmpty || ordered.isEmpty
      slots = taken.toMap
      stated = taken.map(_._1).maxOption.getOrElse(-1) + 1
      // an argument no slot explains is still one the call may need
      asks <- List(false, true)
      width = if (asks) math.max(stated, arity) else stated
      free =
        if (asks)
          (0 until width)
            .filterNot(slots.contains)
            .map(i => Hole.Free(ESValueT, s"$argName$i"))
            .toList
        else Nil
      if asks == free.nonEmpty
      args = (0 until width).toList
        .map(i => "$" + slots.getOrElse(i, s"$argName$i"))
      // emitting both shapes and letting the probe pick costs 71% more build
      expr =
        if (receiver.isDefined || !isCtor)
          s"$surface.call(${(self :: args).mkString(", ")})"
        else Solver.newExpr(surface, args)
      holes = names.map(n => reached.get(n).fold(Hole.Base)(_._1)) ++ free
      // the type states the slot, not the place inside it
      slotted = holes.flatMap(h => h.slot.map(_ -> AnyT))
    } yield Template(RecordT(owner, fixed.toMap ++ slotted), expr, holes)

  // a constant written into a receiver slot carries no value: an atom, not a template
  private lazy val derivedChanges: List[Template] =
    val found = for {
      f <- cfg.funcs
      surface <- Solver.funcAccessExpr(f).toList
      org = holds(f)
      case ICall(_, EClo(g, _), args) <- body(f.name)
      (there, slots) <- writeSummary
        .getOrElse(g, WriteSummary())
        .changed
        .toList
      // a slot the callee fills with a constant carries no caller value
      fixed = slots.collect {
        case (slot, fs) if fs.exists(_.isInstanceOf[Const]) =>
          slot
      }.toSet
      if fixed.nonEmpty
      arg <- args.lift(there).toList
      if sources(arg, org).contains(Receiver)
      // it validates its receiver rather than declaring it
      owner <- receiverOf(fixed)
    } yield RecordT(owner) -> surface
    found.map { (base, s) =>
      Template(
        base,
        s"(() => { const x = $$$baseName; $s.call(x); return x; })()",
        List(Hole.Base),
      )
    }.distinct

  // no builtin path reaches a revocation closure
  private lazy val residualChanges: List[Template] = List(
    Template(
      ObjectT,
      s"(() => { const r = Proxy.revocable($$$baseName, {}); " +
      "r.revoke(); return r.proxy; })()",
      List(Hole.Base),
    ),
  )

  /** what a value can be put through to become another type */
  lazy val stateChanges: List[Template] =
    derivedChanges ++ residualChanges

  // ---------------------------------------------------------------------------
  // follow an argument, and note the slot it lands in
  // ---------------------------------------------------------------------------
  //
  //   for a builtin f and its argument i:
  //     S = { the local holding argument i }
  //     x.slot <- v   for v in S    note the slot, and whose object x is
  //     y <- v        for v in S    S += y
  //     g(.., v, ..)  for v in S    keep following inside g from that parameter
  //
  // a path like MapData.Elem.Key is the same rule applied once more: v lands in
  // p.Key, and p lands in MapData. everything below either follows, or shares
  // the following across the arguments that pass through the same function.

  def calls(f: Func): Map[Local, (String, List[Expr])] =
    spread(
      handOff(f.name),
      body(f.name).collect {
        case ICall(lhs, EClo(g, _), args) => lhs -> (g, args)
      },
    )

  def records(f: Func): Map[Local, ERecord] = body(f.name).collect {
    case Assigned(x, e: ERecord) => x -> e
  }.toMap

  lazy val body: Map[String, List[Inst]] =
    cfg.funcs.map(f => f.name -> stream(f)).toMap

  // a function whose every return hands back a fresh local allocates too

  private lazy val allocating: Set[String] =
    val fs = MSet[String]()
    untilStable(cfg.funcs)(fs.size) { f =>
      if (!fs(f.name)) {
        val fresh = freshIn(f.name, fs)
        val out = returned(f)
        // a record it builds on the spot is as fresh as a local holding one
        def isNew(e: Expr): Boolean = e match
          case ERecord(_, _)  => true
          case ERef(l: Local) => fresh(l)
          case _              => false
        if (out.nonEmpty && out.forall(isNew)) fs += f.name
      }
    }
    fs.toSet

  private def freshIn(fname: String, alloc: String => Boolean): Set[Local] =
    spread(
      handOff(fname),
      body(fname).collect {
        case Assigned(x, ERecord(_, _))            => x -> ()
        case ICall(lhs, EClo(g, _), _) if alloc(g) => lhs -> ()
      },
    ).keySet

  // one local hands its value to another: a copy, a completion, a coercion
  lazy val handOff: Map[String, List[(Local, Local)]] = cfg.funcs.map { f =>
    f.name -> body(f.name).flatMap {
      case Assigned(x, e) => carrier(e).map(_ -> x)
      case ICall(lhs, EClo("NormalCompletion", _), ERef(v: Local) :: Nil) =>
        Some(v -> lhs)
      case ICall(lhs, EClo(g, _), arg :: Nil) if g.startsWith("To") =>
        carrier(arg).map(_ -> lhs)
      case _ => None
    }
  }.toMap

  // everything the seeds reach along those hand-offs, carrying what they hold
  private def spread[T](
    edges: List[(Local, Local)],
    seed: IterableOnce[(Local, T)],
  ): Map[Local, T] =
    val acc = MMap.from(seed)
    untilStable(edges)(acc.size) { (from, to) =>
      if (!acc.contains(to)) acc.get(from).foreach(acc(to) = _)
    }
    acc.toMap

  // sweep until the accumulator stops changing
  private def untilStable[T](over: Iterable[T])(mark: => Any)(
    step: T => Unit,
  ): Unit =
    var last: Any = null
    while (last != mark)
      last = mark
      over.foreach(step)

  // what a function does itself, plus what it inherits from what it calls
  private def summaries[T](step: (Func, MMap[String, T]) => T): Map[String, T] =
    val acc = MMap[String, T]()
    untilStable(cfg.funcs)(acc.toMap)(f => acc(f.name) = step(f, acc))
    acc.toMap

  // every fixpoint below asks this of the same function, so answer it once

  lazy val freshOf: Map[String, Set[Local]] =
    cfg.funcs.map(f => f.name -> freshIn(f.name, allocating)).toMap

  // both sides of an abrupt check often return the same local, so split by node
  private def returned(f: Func): List[Expr] =
    val propagated = f.nodes
      .collect {
        case b: Branch if b.isAbruptNode => b.thenNode.map(_.id)
      }
      .flatten
      .toSet
    for {
      n <- f.nodes.toList if !propagated(n.id)
      case blk: Block <- List(n)
      case IReturn(e) <- blk.insts.toList
    } yield e

  private def handedBack(f: Func): List[Local] =
    returned(f).collect { case ERef(l: Local) => l }

  // the same hand-offs, walked back from what the function returns
  lazy val flowsToReturn: Map[String, Set[Local]] = cfg.funcs.map { f =>
    val back = handOff(f.name).map(_.swap)
    f.name -> spread(back, handedBack(f).map(_ -> ())).keySet
  }.toMap

  // sharing the follow: every argument reaching this function at once, stated
  // against its parameters so that a caller can rename them
  lazy val writeSummary: Map[String, WriteSummary] =
    summaries[WriteSummary] { (f, acc) =>
      val env = holds(f)
      val fresh = freshOf(f.name)
      // a record literal is a creation site as much as an assignment is
      val written = body(f.name).flatMap {
        case Assigned(x, ERecord(_, pairs))          => pairs.map(x -> _)
        case IAssign(Field(b: Local, EStr(slot)), v) => List(b -> (slot, v))
        case _                                       => Nil
      }
      def carrying(keep: Local => Boolean) = written.flatMap { (b, entry) =>
        val (slot, v) = entry
        if (keep(b)) Some(b -> (slot -> sources(v, env))) else None
      }
      val created = carrying(fresh)
        .groupMap(_._1)(_._2)
        .toList
        .sortBy(_._1.toString)
        .map(_._2.toMap)
      val changed = byParam(carrying(!fresh(_)), env)
      val onward = for {
        case ICall(_, EClo(g, _), args) <- body(f.name)
        passed = passedOn(args, env)
        was = acc.getOrElse(g, WriteSummary())
        // an object this function allocated, filled by a call it is handed to
        handed = for {
          (there, fills) <- was.changed.toList.sortBy(_._1)
          case ERef(b: Local) <- args.lift(there).toList
          if fresh(b)
        } yield fills
        mine = (was.created ++ handed).map(hop(_, passed)).filter(_.nonEmpty)
        theirs =
          was.changed.toList.sortBy(_._1).flatMap(rebind(_, passed, hop))
      } yield (mine, theirs)
      WriteSummary(
        created = (created ++ onward.flatMap(_._1)).filter(_.nonEmpty).distinct,
        changed = merge(changed.toList ++ onward.flatMap(_._2)),
      )
    }

  // a value the caller passed has to be retranslated at each hop
  // an input is renamed at each hop; a constant the callee chose travels on
  private def hop(
    slots: Map[String, Set[Val]],
    passed: Map[Int, Set[Val]],
  ): Map[String, Set[Val]] =
    slots
      .map((slot, fs) =>
        slot -> fs.flatMap {
          case Arg(i) => passed.getOrElse(i, Set())
          // the callee's own receiver is not a place we can control
          case Receiver => Set()
          case c        => Set(c)
        },
      )
      .filter(_._2.nonEmpty)

  private def rebind[V](
    entry: (Int, Map[String, V]),
    passed: Map[Int, Set[Val]],
    under: (Map[String, V], Map[Int, Set[Val]]) => Map[String, V],
  ): List[(Int, Map[String, V])] =
    val (at, slots) = entry
    passed
      .getOrElse(at, Set())
      .toList
      // the object it was written on has to be a position the caller controls
      .collect { case Arg(i) => i -> under(slots, passed) }
      .filter(_._2.nonEmpty)

  private def byParam[V](
    pairs: List[(Local, (String, V))],
    env: Map[Local, Set[Val]],
  ): Map[Int, Map[String, V]] =
    merge(for {
      (b, entry) <- pairs
      i <- argsOf(ERef(b), env).toList
    } yield i -> Map(entry))

  private def merge[V](
    pairs: List[(Int, Map[String, V])],
  ): Map[Int, Map[String, V]] =
    pairs.groupMap(_._1)(_._2).view.mapValues(_.reduce(_ ++ _)).toMap

  // the spec names the positions, and a variadic body may index past them
  def argWidth(fname: String): Int =
    cfg.fnameMap.get(fname).fold(0) { f =>
      val declared = f.head.collectFirst { case h: BuiltinHead => h.arity._2 }
      val indexed =
        holds(f).values.flatten.collect { case Arg(i) => i + 1 }
      (declared.getOrElse(0) :: indexed.toList).max
    }

  // when a local came from a property of an argument, not the argument
  def projected(
    f: Func,
    org: Map[Local, Set[Val]],
  ): Map[Local, (Val, String)] =
    spread(
      handOff(f.name),
      for {
        case ICall(lhs, EClo(g, _), args) <- body(f.name)
        (there, prop) <- reads.getOrElse(g, Set()).toList
        obj <- args.lift(there).toList
        place <- sources(obj, org).toList
      } yield lhs -> (place, prop),
    )

  // an operation that hands back one property: which parameter, which key
  private lazy val projects: Map[String, Set[(Int, Int)]] =
    summaries[Set[(Int, Int)]] { (f, acc) =>
      val env = holds(f)
      val out = flowsToReturn(f.name)
      body(f.name).flatMap {
        case ICall(lhs, ERef(Field(o: Local, EStr("Get"))), _ :: key :: _)
            if out(lhs) =>
          for {
            obj <- argsOf(ERef(o), env); k <- argsOf(key, env)
          } yield obj -> k
        case ICall(lhs, EClo(g, _), args) if out(lhs) =>
          for {
            (there, key) <- acc.getOrElse(g, Set()).toList
            passed = passedOn(args, env)
            case Arg(obj) <- passed.getOrElse(there, Set()).toList
            case Arg(k) <- passed.getOrElse(key, Set()).toList
          } yield obj -> k
        case _ => Nil
      }.toSet
    }

  // the same once a caller has pinned the key to a literal
  private lazy val reads: Map[String, Set[(Int, String)]] = (for {
    f <- cfg.funcs
    env = holds(f)
    out = flowsToReturn(f.name)
    case ICall(lhs, EClo(g, _), args) <- body(f.name)
    if out(lhs)
    (there, key) <- projects.getOrElse(g, Set()).toList
    obj <- args.lift(there).toList.flatMap(argsOf(_, env))
    case EStr(prop) <- args.lift(key).toList
  } yield f.name -> (obj, prop))
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap

  /** which env a local may hold: a local gains, and never loses, one */
  def holds(f: Func): Map[Local, Set[Val]] =
    holdsOf.getOrElseUpdate(f.name, holdsIn(f))

  private val holdsOf = TrieMap[String, Map[Local, Set[Val]]]()

  private def holdsIn(f: Func): Map[Local, Set[Val]] =
    val lists = listAliases(f)
    val args = argSlots(f, lists)
    val acc = MMap[Local, Set[Val]]().withDefaultValue(Set())
    // a builtin pops its inputs; an operation declares them in the IR
    for ((p, i) <- f.irFunc.params.zipWithIndex)
      if (!f.head.exists(_.isInstanceOf[BuiltinHead]))
        acc(p.lhs) = Set(Arg(i))
    untilStable(body(f.name))(acc.values.map(_.size).sum) { i =>
      transfer(i, acc, lists, args).foreach((x, v) => acc(x) = acc(x) ++ v)
    }
    acc.toMap

  // a variadic head hands over the whole list, which the body may rename
  private def listAliases(f: Func): Set[Local] =
    spread(handOff(f.name), List(Name("ArgumentsList") -> ())).keySet

  // an argument is popped one at a time, and its place is a static fact
  private def argSlots(f: Func, lists: Set[Local]): Map[Local, Int] =
    // a variadic parameter is the whole list, so no pop ever binds it
    val places = f.head
      .collectFirst { case h: BuiltinHead => h.params }
      .getOrElse(Nil)
      .zipWithIndex
      .collect { case (p, i) if p.kind != ParamKind.Variadic => p.name -> i }
      .toMap
    val acc = MMap[Local, Int]()
    var idx = 0
    for (i <- body(f.name)) i match
      case IPop(x, ERef(l: Local), _) if lists(l) =>
        acc(x) = x match
          case Name(n) if places.contains(n) => places(n)
          case _                             => idx
        idx += 1
      case _ => ()
    acc.toMap

  private def transfer(
    i: Inst,
    st: collection.Map[Local, Set[Val]],
    lists: Set[Local],
    args: Map[Local, Int],
  ): Option[(Local, Set[Val])] = i match
    case IPop(x: Local, _, _) if args.contains(x) =>
      Some(x -> Set(Arg(args(x))))
    case Assigned(x, e) => Some(x -> sources(e, st, lists))
    // a coercion restates a value; a predicate and an allocation do not. the
    // three structural stand-ins measured for this all lost templates
    case ICall(lhs, EClo(g, _), arg :: Nil) if g.startsWith("To") =>
      Some(lhs -> sources(arg, st, lists))
    case _ => None

  // ---------------------------------------------------------------------------
  // reading a call off the IR
  // ---------------------------------------------------------------------------

  // a callee states its places against its own list, so each is looked up
  private def passedOn(
    args: List[Expr],
    env: Map[Local, Set[Val]],
  ): Map[Int, Set[Val]] =
    args.zipWithIndex.map((e, i) => i -> sources(e, env)).toMap

  private def stream(f: Func): List[Inst] =
    f.nodes.toList.sortBy(_.id).flatMap {
      case b: Block => b.insts.toList
      case c: Call  => List(c.callInst)
      case _        => Nil
    }
}

object TemplateDeriver {

  // a template wants the most specific owner, a receiver the most general
  def ownerOf(
    slots: Set[String],
    fixed: Map[String, ValueTy] = Map(),
  ): List[String] = owners(slots, fixed, true)

  def receiverOf(
    slots: Set[String],
    fixed: Map[String, ValueTy] = Map(),
  ): List[String] = owners(slots, fixed, false)

  private def owners(
    slots: Set[String],
    fixed: Map[String, ValueTy],
    specific: Boolean,
  ): List[String] =
    val model = ManualInfo.tyModel
    val all = model.decls.map(_.name).filter { n =>
      // what the type itself declares, not what some subtype might add
      val fs = model.upperFieldsOf(n)
      slots.forall(s => fs.get(s).exists(!_.value.isBottom)) &&
      fixed.forall((s, t) => fs.get(s).forall(f => !(f.value && t).isBottom))
    }
    all.filterNot(n =>
      all.exists(m =>
        m != n &&
        (if (specific) model.isStrictSubTy(m, n)
         else model.isStrictSubTy(n, m)),
      ),
    )

  // a constructor is the one that does something with the NewTarget it is given
  def usesNewTarget(f: Func): Boolean =
    var found = false
    val walker = new UnitWalker {
      override def walk(ref: Ref): Unit = ref match
        case Name(n) if n == NEW_TARGET_STR => found = true
        case _                              => super.walk(ref)
    }
    f.nodes.foreach {
      case b: Block   => b.insts.foreach(walker.walk)
      case c: Call    => walker.walk(c.callInst)
      case br: Branch => walker.walk(br.cond)
    }
    found

  /** a let and an assignment both bind a local to a value */
  object Assigned:
    def unapply(i: Inst): Option[(Local, Expr)] = i match
      case ILet(x, e)           => Some(x -> e)
      case IAssign(x: Local, e) => Some(x -> e)
      case _                    => None

  // a local carries a value along, and a completion carries what it wraps
  private def carrier(e: Expr): Option[Local] = e match
    case ERef(l: Local)                       => Some(l)
    case ERef(Field(l: Local, EStr("Value"))) => Some(l)
    case _                                    => None

  /** just the argument positions among them */
  def argsOf(e: Expr, env: collection.Map[Local, Set[Val]]): Set[Int] =
    sources(e, env).collect { case Val.Arg(i) => i }

  /** what a value may carry; several, and each is a template of its own */
  def sources(
    e: Expr,
    env: collection.Map[Local, Set[Val]],
    lists: Set[Local] = Set(),
  ): Set[Val] = e match
    // the receiver may be handed on directly, never binding a local
    case ERef(Name("this")) => Set(Val.Receiver)
    // an aliased argument list may be indexed instead of popped
    case ERef(Field(l: Local, EMath(i))) if lists(l) && i.isValidInt =>
      Set(Val.Arg(i.toInt))
    // a local carries a value along, and a completion carries what it wraps
    case ERef(Field(l: Local, EStr("Value"))) => env.getOrElse(l, Set())
    case ERef(l: Local)                       => env.getOrElse(l, Set())
    case EEnum(name)                          => Set(Val.Const(EnumT(name)))
    case ENull()                              => Set(Val.Const(NullT))
    case EStr(str)                            => Set(Val.Const(StrT(str)))
    case _                                    => Set()
}
