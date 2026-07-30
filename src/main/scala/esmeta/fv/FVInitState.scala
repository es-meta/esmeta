package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.*
import esmeta.extractor.Extractor
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => IRFunc}
import esmeta.state.*
import esmeta.util.BaseUtils.optional
import esmeta.util.SystemUtils.*
import scala.collection.mutable.ListBuffer

/** Export the ECMAScript specification and the initial state it runs from
  * into a single Rocq file (`formal/validation/Spec.v`), so the model can
  * execute a real script rather than a hand-written IR program.
  *
  * Emits, as one `prog`:
  *   - every spec function `FVExport` can translate (the rest are omitted
  *     and counted; a call to a missing function is UB in the model, which
  *     is honest — it is a function we do not have);
  *   - the initial globals and heap from `Initialize(cfg).from(source)`;
  *   - the source text and the cached AST, with `subIdx`, printed source
  *     and lexical-SDO answers precomputed per node (ADR-12, ADR-15).
  *
  * ADDRESSES (ADR-16). ESMeta's initial heap uses only `NamedAddr`s, and
  * its dynamic-allocation counter starts at 0. The model has `nat`
  * addresses only, so every initial address is renumbered to its position
  * in the exported heap list and the model's counter starts at the heap
  * length. This is a bijection on addresses, and nothing in the model
  * observes an address's numeric value (`IPrint` payloads carrying
  * addresses are excluded by the observable-behaviour spec, L-6), so
  * equality — the only thing the semantics does with addresses — is
  * preserved.
  *
  * Usage: sbt "runMain esmeta.fv.FVInitState [source]"
  */
object FVInitState {

  import FVExport.{Unsupported, coqList, strLit, cstrLit, zLit, floatLit}

  /** the six lexical SDOs (Interpreter.scala:525-536) */
  val LEX_SDOS =
    List("StringValue", "NumericValue", "MV", "SV", "TV", "TRV")

  def main(args: Array[String]): Unit = {
    val source = if (args.nonEmpty) args(0) else "var x = 1;"
    println("[fv] extracting spec and building CFG")
    val cfg = CFGBuilder(Compiler(Extractor()))
    given CFG = cfg
    val st = Initialize(cfg).from(source)

    // ---- addresses: Addr -> list position (ADR-16) -------------------
    // Every address REFERENCED anywhere gets a slot, even one the heap
    // does not map (ESMeta's initial globals contain #CandidateExecution,
    // which is dangling): the slot exists so allocation cannot later reuse
    // that index, but it holds None so dereferencing is stuck, matching
    // ESMeta's UnknownAddr.
    def refsOf(v: Value): List[Addr] = v match
      case a: Addr        => List(a)
      case Clo(_, cap)    => cap.values.toList.flatMap(refsOf)
      case _              => Nil
    val referenced = (
      st.globals.values.toList.flatMap(refsOf) ++
      st.heap.map.values.toList.flatMap {
        case RecordObj(_, m) => m.values.toList.flatMap(refsOf)
        case ListObj(vs)     => vs.toList.flatMap(refsOf)
        case MapObj(m) => m.toList.flatMap((k, v) => refsOf(k) ++ refsOf(v))
        case _               => Nil
      }
    ).toSet
    val mapped = st.heap.map.keySet.toSet
    val dangling = (referenced -- mapped).toList
    val ord: Addr => (Int, String, Long) = {
      case NamedAddr(n)   => (0, n, 0L)
      case DynamicAddr(l) => (1, "", l)
    }
    val addrs = mapped.toList.sortBy(ord) ++ dangling.sortBy(ord)
    val addrIdx: Map[Addr, Int] = addrs.zipWithIndex.toMap
    if (dangling.nonEmpty)
      println(s"[fv] referenced but unmapped addresses (slot = None): " +
        dangling.sortBy(ord).mkString(", "))
    if (st.heap.size != 0)
      println(s"[fv] WARNING: heap counter is ${st.heap.size}, not 0")

    def value(v: Value): String = v match
      case addr: Addr =>
        addrIdx.get(addr) match
          case Some(i) => s"(VAddr $i)"
          case None    => throw Unsupported(s"unmapped address: $addr")
      case Clo(f, captured) =>
        val cs = captured.toList.sortBy(_._1.name).map { (n, cv) =>
          s"(${strLit(n.name)}, ${value(cv)})"
        }
        s"(VClo ${strLit(f.name)} ${coqList(cs)})"
      case Math(d) =>
        if (!d.isWhole) throw Unsupported(s"non-integer Math value: $d")
        s"(VMath ${zLit(d.toBigInt)})"
      case Bool(b)      => s"(VBool $b)"
      case Str(s)       => s"(VStr ${cstrLit(s)})"
      case Undef        => "VUndef"
      case Null         => "VNull"
      case Enum(n)      => s"(VEnum ${strLit(n)})"
      case Number(d)    => s"(VNumber ${floatLit(d)})"
      case BigInt(n)    => s"(VBigInt ${zLit(n)})"
      case CodeUnit(c)  => s"(VCodeUnit ${c.toInt})"
      case Infinity(p)  => s"(VInfinity $p)"
      case GrammarSymbol(n, ps) =>
        s"(VGrammarSymbol ${strLit(n)} ${coqList(ps.map(_.toString))})"
      case AstValue(a)  => s"(VAst ${ast(a)})"
      case _ => throw Unsupported(s"state value: ${v.getClass.getSimpleName}")

    def obj(o: Obj): String = o match
      case RecordObj(tname, m) =>
        val fs = m.toList.map { (f, v) => s"(${strLit(f)}, ${value(v)})" }
        s"(ORecord ${strLit(tname)} ${coqList(fs)})"
      case ListObj(vs) => s"(OList ${coqList(vs.toList.map(value))})"
      case MapObj(m) =>
        val es = m.toList.map { (k, v) => s"(${value(k)}, ${value(v)})" }
        s"(OMap ${coqList(es)})"
      case _ => throw Unsupported(s"obj: ${o.getClass.getSimpleName}")

    /** an AST node with everything grammar-derived precomputed */
    def ast(a: Ast): String = a match
      case lex @ Lexical(name, str) =>
        val tbl = LEX_SDOS.flatMap { m =>
          optional(Interpreter.eval(lex, m)).map { v =>
            val lv = v match
              case Str(s)    => s"(LVStr ${cstrLit(s)})"
              case Math(d) if d.isWhole => s"(LVMath ${zLit(d.toBigInt)})"
              case Number(d) => s"(LVNumber ${floatLit(d)})"
              case BigInt(n) => s"(LVBigInt ${zLit(n)})"
              case other =>
                throw Unsupported(s"lexical SDO $m -> $other")
            s"(${strLit(m)}, $lv)"
          }
        }
        val src = a.toString(grammar = Some(cfg.grammar)).trim
        s"(ALex ${strLit(name)} ${strLit(str)} ${cstrLit(src)} ${coqList(tbl)})"
      case syn @ Syntactic(name, sargs, rhsIdx, children) =>
        val cs = children.toList.map {
          case Some(c) => s"(Some ${ast(c)})"
          case None    => "None"
        }
        val src = a.toString(grammar = Some(cfg.grammar)).trim
        s"(ASyn ${strLit(name)} ${coqList(sargs.map(_.toString))} " +
        s"$rhsIdx ${syn.subIdx} ${coqList(cs)} ${cstrLit(src)})"

    // Anything the model cannot represent faithfully is made STUCK, never
    // approximated: an object with unrepresentable content becomes an
    // unmapped slot, and an unrepresentable global is omitted.  Touching
    // either is then undefined behaviour instead of a wrong answer.
    def tryEmit(f: => String): Option[String] =
      try Some(f) catch { case Unsupported(_) => None }

    // ---- spec functions ----------------------------------------------
    val funcDefs = ListBuffer[(String, String)]()
    var skipped = 0
    val skipReasons = scala.collection.mutable.Map[String, Int]()
    for ((f, i) <- cfg.program.funcs.zipWithIndex)
      try funcDefs += ((s"sf_$i", FVExport.rocqFunc(f)))
      catch {
        case Unsupported(msg) =>
          skipped += 1
          val k = msg.takeWhile(_ != ':')
          skipReasons(k) = skipReasons.getOrElse(k, 0) + 1
      }
    println(s"[fv] spec functions: ${funcDefs.size} exported, $skipped omitted")
    val mainF = cfg.program.funcs.filter(_.main)
    println(s"[fv] main function(s): " + mainF.map(f =>
      s"${f.name} params=${f.params.size} " +
      (if (scala.util.Try(FVExport.rocqFunc(f)).isSuccess) "EXPORTED"
       else "OMITTED")).mkString(", "))
    for ((k, n) <- skipReasons.toList.sortBy(-_._2))
      println(f"[fv]   $n%5d  $k")

    // ---- assemble -----------------------------------------------------
    val sb = new StringBuilder
    sb ++= s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState"`.
 *
 * The ECMAScript specification as an IR-Core program, plus the initial
 * state a script starts from.  See ADR-16 for the address renumbering.
 *
 * source text : ${strLit(source)}
 * spec funcs  : ${funcDefs.size} exported, $skipped omitted
 * heap        : ${mapped.size} objects, ${dangling.size} unmapped slots
 * globals     : ${st.globals.size}
 *)
From Stdlib Require Import String ZArith List Floats.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain Exec.
Local Open Scope string_scope.
Local Open Scope Z_scope.

"""
    for ((n, body) <- funcDefs) sb ++= s"Definition $n : func :=\n  $body.\n"
    sb ++= s"\nDefinition spec_funcs : list func :=\n  " +
      coqList(funcDefs.toList.map(_._1)) + ".\n\n"

    val gPairs = st.globals.toList.sortBy(_._1.name).map { (g, v) =>
      g.name -> tryEmit(s"(${strLit(g.name)}, ${value(v)})")
    }
    val droppedGlobals = gPairs.collect { case (n, None) => n }
    if (droppedGlobals.nonEmpty)
      println("[fv] globals omitted as unrepresentable (reads are stuck): " +
        droppedGlobals.mkString(", "))
    sb ++= s"Definition init_globals : list (string * val) :=\n  " +
      coqList(gPairs.flatMap(_._2)) + ".\n\n"

    var droppedObjs = 0
    val heapTerms = addrs.map { a =>
      st.heap.map.get(a) match
        case None => "None"
        case Some(o) =>
          tryEmit(s"(Some ${obj(o)})").getOrElse { droppedObjs += 1; "None" }
    }
    if (droppedObjs > 0)
      println(s"[fv] heap objects unrepresentable, slot left unmapped: " +
        s"$droppedObjs")
    sb ++= "Definition init_heap : list (option obj) :=\n  " +
      coqList(heapTerms) + ".\n\n"

    val srcTerm = s"(Some ${cstrLit(source)})"
    val astTerm = st.cachedAst.fold("None")(a => s"(Some ${ast(a)})")
    sb ++= s"Definition spec_prog : prog :=\n" +
      s"  mkProgFull spec_funcs $srcTerm $astTerm init_globals init_heap.\n"

    val out = s"$BASE_DIR/formal/validation/Spec.v"
    dumpFile(sb.toString, out)
    println(s"[fv] wrote $out (${sb.length / 1024} KiB)")

    // ---- which spec functions does the run actually enter? -------------
    // Cheapest way to find out whether an omitted function is the reason
    // the model gets stuck: ask ESMeta, which can run the source.
    val visited = scala.collection.mutable.Set[String]()
    val probeSt = Initialize(cfg).from(source)
    val probe = new Interpreter(probeSt, timeLimit = Some(60)) {
      override def step: Boolean =
        visited += this.st.context.func.name
        super.step
    }
    try probe.result
    catch { case _: Throwable => () }
    val omittedNames = cfg.program.funcs
      .filter(f => scala.util.Try(FVExport.rocqFunc(f)).isFailure)
      .map(_.name)
      .toSet
    val hit = visited.toSet & omittedNames
    println(s"[fv] functions entered by this run: ${visited.size}")
    println(s"[fv] of those, omitted from the export: ${hit.size}")
    val byName = cfg.program.funcs.map(f => f.name -> f).toMap
    for (n <- hit.toList.sorted) {
      val rs = FVSpecScan.blockers(byName(n))
      println(s"[fv]   $n  <-  ${rs.toList.sorted.mkString(", ")}")
    }
    // the whole reachable set, so the work list is reachability-driven
    val allReasons = scala.collection.mutable.Map[String, Int]()
    for (n <- hit) for (r <- FVSpecScan.blockers(byName(n)))
      allReasons(r) = allReasons.getOrElse(r, 0) + 1
    println("[fv] blockers on the reachable set, by function count:")
    for ((r, c) <- allReasons.toList.sortBy(-_._2))
      println(f"[fv]   $c%3d  $r")

    // ---- ESMeta's own run of the same source: the differential oracle --
    // A SEPARATE file so Spec.v's compile cost can be measured on its own.
    // Uses a fresh initial state; the one above must stay pristine.
    val runSt = Initialize(cfg).from(source)
    val interp = new FVExport.CapturingInterpreter(runSt)
    val t0 = System.nanoTime()
    val outcome =
      try {
        val fin = interp.result
        val res = fin.globals.getOrElse(GLOBAL_RESULT, Undef)
        Right((res, interp.prints.toList))
      } catch { case e: Throwable => Left(e.toString.take(200)) }
    val ms = (System.nanoTime() - t0) / 1000000
    outcome match
      case Left(err) =>
        println(s"[fv] ESMeta could not run the source (${ms} ms): $err")
        println("[fv] no SpecRun.v emitted")
      case Right((res, prints)) =>
        println(s"[fv] ESMeta ran the source in ${ms} ms; " +
          s"RESULT=$res, ${prints.size} print(s)")
        val terms = tryEmit(
          s"Ok (${value(res)}, ${coqList(prints.map(value))})",
        )
        terms match
          case None =>
            println("[fv] outcome not representable; no SpecRun.v emitted")
          case Some(expected) =>
            val r = new StringBuilder
            r ++= s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState"`.
 *
 * Differential check for stage G4/G5: ESMeta ran ${strLit(source)} from the
 * exported initial state and produced the observable below; compiling this
 * file checks that the Rocq reference interpreter agrees.
 *)
From Stdlib Require Import String ZArith List Floats.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain Exec.
From ESMetaFV Require Import Spec.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Example spec_run_ok : run 10000000 spec_prog = $expected.
Proof. vm_compute. reflexivity. Qed.
"""
            val ro = s"$BASE_DIR/formal/validation/SpecRun.v"
            dumpFile(r.toString, ro)
            println(s"[fv] wrote $ro")
  }
}
