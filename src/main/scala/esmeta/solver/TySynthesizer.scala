package esmeta.solver

import esmeta.cfg.CFG
import esmeta.es.builtin.{INNER_MAP, REALM}
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.ManualInfo
import scala.collection.concurrent.{Map => CMap, TrieMap}

/** programs of a demanded type, assembled from atoms and templates */
class TySynthesizer(cfg: CFG) {

  import TySynthesizer.*
  import Hole.*

  private val templateDeriver = TemplateDeriver(cfg)

  /** programs whose observed type the obligation admits */
  def candidates(ty: ValueTy): LazyList[String] =
    (pinned(ty).to(LazyList) #::: fromAtoms(ty) #::: fromStructure(ty) #:::
      fromConstruct(ty).to(LazyList) #::: fromCall(ty).to(LazyList) #:::
      fromTemplate(ty) #::: LazyList.from(atomDeriver.literalsFor(ty))).distinct

  private def exprFor(ty: ValueTy): Option[String] = candidates(ty).headOption

  // ---------------------------------------------------------------------------
  // atoms the type names outright
  // ---------------------------------------------------------------------------

  private def pinned(ty: ValueTy): List[String] =
    val numbers = ty.number.toNumberSet.fold(Nil) { set =>
      set.toList.sortBy(n => (n.isNaN, n.double)).map(AtomDeriver.numberLit)
    }
    numbers ++
    ty.bigInt.map(n => s"${n}n").toList ++
    ty.str.map(str => s"\"${normStr(str)}\"").toList ++
    ty.bool.set.toList.sorted.map(b => if (b) "true" else "false") ++
    (if (ty.undef) List("undefined") else Nil) ++
    (if (ty.nullv) List("null") else Nil)

  private val atomDeriver = AtomDeriver(cfg)

  // ---------------------------------------------------------------------------
  // templates the obligation drives
  // ---------------------------------------------------------------------------

  // values built from the object shape a type carries
  private def fromStructure(ty: ValueTy): LazyList[String] = ty.record match
    case RecordTy.Elem(map, ObjShape(props, call, construct))
        if props.nonEmpty =>
      val ordered = props.toList.sortBy(_._1)
      val literals = objectLiterals(ordered)
      if (isPlainObject(ty)) literals
      else
        val bare =
          ty.copied(record =
            RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
          )
        exprFor(bare).fold(LazyList.empty) { b =>
          proxies(ordered, b) #::: literals.map { o =>
            s"Object.defineProperties($b, Object.getOwnPropertyDescriptors($o))"
          }
        }
    case _ => LazyList.empty

  private def objectLiterals(
    ordered: List[(Property, Desc)],
  ): LazyList[String] =
    val members = ordered.map { (prop, desc) =>
      val k = propKey(prop)
      if (desc.getExc) LazyList(s"get $k() { throw 0; }")
      else if (desc.setExc) LazyList(s"set $k(_) { throw 0; }")
      else candidates(desc.ty).map(v => s"$k: $v")
    }
    oneChange(members).map(_.mkString("{ ", ", ", " }"))

  // one trap answers every property, so the shape may name more than one
  private def proxies(
    ordered: List[(Property, Desc)],
    base: String,
  ): LazyList[String] =
    val (written, read) = ordered.partition((_, d) => d.setExc)
    val clauses = read.map { (prop, desc) =>
      val key = propExpr(prop)
      if (desc.getExc) LazyList(s"if (p === $key) throw 0;")
      else candidates(desc.ty).map(v => s"if (p === $key) return $v;")
    }
    val getting =
      if (read.isEmpty) LazyList("")
      else
        oneChange(clauses).map { cs =>
          s"get(t, p, r) { ${cs.mkString(" ")} return Reflect.get(t, p, r); }"
        }
    val setting =
      if (written.isEmpty) ""
      else
        val test =
          written.map((p, _) => s"p === ${propExpr(p)}").mkString(" || ")
        s"set(t, p, v, r) { if ($test) throw 0; return Reflect.set(t, p, v, r); }"
    getting.map { g =>
      s"new Proxy($base, { ${List(g, setting).filter(_.nonEmpty).mkString(", ")} })"
    }

  // an arrow has no [[Construct]], so a constructor must be written out
  private def closure(
    exc: Boolean,
    ret: ValueTy,
    arrow: Boolean,
  ): Option[String] =
    if (exc)
      Some(if (arrow) "() => { throw 0; }" else "function() { throw 0; }")
    else
      exprFor(ret).map { v =>
        if (arrow) s"() => ($v)" else s"function() { return $v; }"
      }

  private def fromConstruct(ty: ValueTy): Option[String] =
    ty.record.construct match
      case ConstructDesc.Elem(exc, ret) => closure(exc, ret, arrow = false)
      case ConstructDesc.Top            => None

  private def fromCall(ty: ValueTy): Option[String] =
    ty.record.call match
      case CallDesc.Elem(exc, ret) =>
        closure(exc, ret, arrow = !(ty <= ConstructorT))
      case CallDesc.Top => None

  private def isPlainObject(ty: ValueTy): Boolean = ty.record match
    case RecordTy.Elem(map, _) =>
      ObjectT ⊑ ty.copied(record = RecordTy.Elem(map))
    case _ => ObjectT ⊑ ty

  // an atom stands as written, so its own type must prove the obligation
  private def fromAtoms(ty: ValueTy): LazyList[String] =
    lookup(ty, atoms.filter(_.ty <= ty), cachedAtoms)

  private def fromTemplate(ty: ValueTy): LazyList[String] =
    lookup(ty, templates.filter(applies(_, ty)), cachedTemplates)

  // a template's type settles only once its slots are filled
  private def applies(t: Template, ty: ValueTy): Boolean =
    t.holes.forall(_.slot.forall(bound(ty, _))) && filled(t, ty) <= ty

  private def filled(t: Template, ty: ValueTy): ValueTy =
    val record = t.holes.foldLeft(t.ty.record) { (r, h) =>
      h.slot.fold(r)(s => r.update(s, ty.record(s).value, refine = true))
    }
    t.ty.copied(record = record)

  private def prioritize(ts: List[Template], ty: ValueTy): LazyList[String] =
    ts.to(LazyList).flatMap(fill(_, ty)).distinct

  // longest first, so one hole cannot clobber another's prefix
  private def substituted(
    t: Template,
    choices: List[(String, LazyList[String])],
  ): LazyList[String] =
    val (names, alts) = choices.sortBy((n, _) => -n.length).unzip
    oneChange(alts).map { chosen =>
      names.zip(chosen).foldLeft(t.expr) {
        case (acc, (n, e)) => acc.replace("$" + n, e)
      }
    }

  private def fill(t: Template, ty: ValueTy): LazyList[String] =
    substituted(t, slotChoices(t, ty))

  private def lookup(
    ty: ValueTy,
    ts: => List[Template],
    cache: CMap[ValueTy, LazyList[String]],
  ): LazyList[String] =
    if (ty.isBottom) LazyList.empty
    else cache.getOrElseUpdate(ty, prioritize(ts, ty))

  private val cachedAtoms = TrieMap[ValueTy, LazyList[String]]()
  private val cachedTemplates = TrieMap[ValueTy, LazyList[String]]()

  private def slotChoices(
    t: Template,
    ty: ValueTy,
  ): List[(String, LazyList[String])] =
    t.holes.map { h =>
      h.name -> (h match
        // finished programs only, or filling would ask for this type again
        case Hole.Base          => fromAtoms(t.ty)
        case Hole.Free(free, _) => candidates(free)
        case Hole.Slot(slotName, sk, src) =>
          candidates(holeTy(ty.record(slotName).value, sk, src))
      )
    }

  private def bound(ty: ValueTy, slot: String): Boolean = ty.record match
    case RecordTy.Elem(map, _) => map.exists((_, fm) => !fm(slot).isTop)
    case _                     => false

  private def runAll(exprs: List[String]): Map[String, ValueTy] =
    exprs.grouped(120).flatMap(runBatch).toMap

  private def runBatch(exprs: List[String]): Map[String, ValueTy] =
    if (exprs.isEmpty) Map()
    else
      try harvested(exprs)
      catch {
        case _: Throwable if exprs.size > 1 =>
          val (l, r) = exprs.splitAt(exprs.size / 2)
          runBatch(l) ++ runBatch(r)
        case _: Throwable => Map()
      }

  // one run binds every expression, so one that throws cannot lose the rest
  private def harvested(exprs: List[String]): Map[String, ValueTy] =
    val src = exprs.zipWithIndex
      .map((e, i) => s"var __w${i}__; try { __w${i}__ = ($e); } catch (e) {}")
      .mkString("\n")
    val st = Interpreter(cfg.init.from(src), timeLimit = Some(20))
    val bound = for {
      realm <- st.get(Global(REALM))
      global <- st.get(realm, Str("GlobalObject"))
      map <- st.get(global, Str(INNER_MAP))
    } yield map
    (for {
      map <- bound.toOption.toList
      (e, i) <- exprs.zipWithIndex
      v <- st
        .get(map, Str(s"__w${i}__"))
        .flatMap(st.get(_, Str("Value")))
        .toOption
      ty = observedTy(st, v)
      if !ty.isBottom && !ty.record.isBottom
    } yield e -> ty).toMap

  // an expression that carries no declared type is what it ran as
  private def observedAs(exprs: List[String]): List[Template] =
    val uniq = exprs.distinct
    val obs = runAll(uniq)
    uniq.flatMap(e => obs.get(e).map(Template(_, e)))

  // a template keeps its declared type unless running it proves a tighter one
  private def tightened(ts: List[Template]): List[Template] =
    val obs = runAll(ts.map(_.expr).distinct)
    ts.map { t =>
      val seen = obs.get(t.expr).filter(_ <= t.ty)
      t.copy(ty = seen.getOrElse(t.ty))
    }

  // ---------------------------------------------------------------------------
  // building the table
  // ---------------------------------------------------------------------------

  private def folded(ts: List[(String, Template)]): List[(String, Template)] =
    def key(r: (String, Template)) = (r._2.ty, r._2.holes.map(_.name).toSet)
    val best = ts.groupMapReduce(key)(identity) { (l, r) =>
      if (l._2.expr.length <= r._2.expr.length) l else r
    }
    ts.map(key).distinct.map(best)

  // a derived template is wrong until running it says otherwise
  private lazy val derived: List[Template] =
    val filled =
      templateDeriver.fromWrites.distinct
        .map {
          case (o, r) => (o, r, probe(o, r))
        }
    val obs = runAll(filled.flatMap(_._3).distinct)
    folded(for {
      (owner, t, js) <- filled
      proof = js.flatMap(e => obs.get(e).filter(_ <= t.ty).map(e -> _))
      if proof.nonEmpty
      // an obligation constraining no slot would never let the template fire
      out <-
        if (t.holes.nonEmpty)
          t :: proof.map((e, seen) => Template(seen, e))
        else List(t.copy(ty = proof.head._2))
    } yield owner -> out).map(_._2)

  private def probe(owner: String, t: Template): List[String] =
    // a wrapped hole has no atom of its own, only the value inside does
    val choices = t.holes.map { h =>
      h.name -> (h match
        case Hole.Slot(into, sk, Some(prop)) =>
          probesFor(declaredAt(owner, Hole.Slot(into, sk)))
            .map(v => s"{ $prop: $v }")
        case _ => probesFor(declaredAt(owner, h))
      )
    }
    substituted(t, choices).take(maxFillings).toList

  // the same synthesis, minus the sources that would recur
  private def probesFor(ty: ValueTy): LazyList[String] =
    (bootstrap.to(LazyList).filter(_.ty <= ty).map(_.expr) #:::
      LazyList.from(pinned(ty)) #:::
      LazyList.from(atomDeriver.literalsFor(ty))).distinct.take(maxProbes)

  // what the spec writes down, typed by running it
  private lazy val vocabulary: List[Template] =
    tightened(atomDeriver.fromSyntax) ++ observedAs(atomDeriver.constructions)

  // the same, put through a change that leaves it another type
  private lazy val altered: List[Template] =
    shortest(observedAs(for {
      change <- templateDeriver.stateChanges
      atom <- vocabulary if atom.ty <= change.ty
    } yield change.expr.replace("$" + baseName, atom.expr)))

  // what exists before the table does; a literal joins on demand in `candidates`
  private lazy val bootstrap: List[Template] = vocabulary ++ altered

  // an atom is a template with nothing left to fill, so the table is both
  private lazy val library: List[Template] =
    (bootstrap ++ derived).distinct

  private lazy val atoms: List[Template] =
    library.filter(_.holes.isEmpty)

  private lazy val templates: List[Template] =
    library.filterNot(_.holes.isEmpty)
}

object TySynthesizer {

  import Hole.*

  // ---------------------------------------------------------------------------
  // helpers that need no spec, only what they are handed
  // ---------------------------------------------------------------------------

  private def propExpr(prop: Property): String = prop match
    case Property.PStr(str) => s"\"${normStr(str)}\""
    case Property.PSym(sym) => s"Symbol.$sym"

  private def propKey(prop: Property): String = s"[${propExpr(prop)}]"

  // one template per type, the shortest, so a blind sweep stays small
  private def shortest(ts: List[Template]): List[Template] =
    ts
      .groupMapReduce(_.ty)(identity) { (l, r) =>
        if (l.expr.length <= r.expr.length) l else r
      }
      .values
      .toList
      .sortBy(_.expr)

  private def declaredAt(owner: String, h: Hole): ValueTy =
    h match
      case Hole.Base                => RecordT(owner)
      case Hole.Free(ty, _)         => ty
      case Hole.Slot(into, sk, src) => holeTy(declared(owner, into), sk, src)

  private def declared(tname: String, field: String): ValueTy =
    ManualInfo.tyModel.getField(tname, field).value

  private val maxProbes = 3

  private val maxFillings = 4

  // a field that differs per instance says nothing about which record it is
  private val opaque =
    Set(
      "Extensible",
      "SourceText",
      "InitialName",
      "__CODE__",
      "PrivateElements",
    )

  private def refined(ty: RecordTy, fields: List[(String, ValueTy)]): RecordTy =
    fields.iterator
      .map((f, t) => (f, t, ty.update(f, t, refine = true)))
      .filter((f, t, n) => !n.isBottom && !emptied(t, n(f).value))
      .minByOption((_, _, n) =>
        n.names match
          case Fin(set) => set.size
          case _        => Int.MaxValue,
      ) match
      case None            => ty
      case Some((f, _, n)) => refined(n, fields.filterNot(_._1 == f))

  // an element the model rejects reads as an empty list, which was never seen
  private def emptied(seen: ValueTy, got: ValueTy): Boolean =
    got.list == ListTy.Nil && seen.list != ListTy.Nil

  // what the spec built before the program ran is machinery, not observation
  private def observation(st: State, x: Value): Boolean = x match
    case addr: Addr => !st.cfg.init.initHeap.map.contains(addr)
    case _          => true

  // the state's own typing drops a primitive to its whole kind
  private def exactly(st: State, v: Value): ValueTy = v match
    case Str(s)    => StrT(s)
    case Bool(b)   => BoolT(Set(b))
    case n: Number => NumberT(n.double)
    case _         => st.typeOf(v, detail = true)

  private def observedTy(st: State, v: Value): ValueTy = v match
    case addr: Addr =>
      st.heap.map.get(addr) match
        case Some(RecordObj(tname, map)) =>
          val fields = map.iterator
            .filter((f, x) => !opaque(f) && observation(st, x))
            .map((f, x) => f -> exactly(st, x))
            .toList
          ValueTy(record = refined(RecordTy(tname), fields))
        case Some(o) => st.typeOf(o, detail = false)
        case None    => BotT
    case _ => BotT
}
