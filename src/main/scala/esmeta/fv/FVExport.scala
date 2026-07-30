package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

/** Exporter for the formal-verification differential harness (PO-011).
  *
  * For each fragment-compatible standalone IR program, this tool:
  *   - translates the program into a Rocq term of the `ESMetaFV.Fragment`
  *     datatypes (mirrored in `formal/Fragment.v`);
  *   - executes the program with ESMeta's own interpreter, capturing the
  *     `IPrint` values and the final `RESULT` global;
  *   - emits a `vm_compute`-checked expectation that the Rocq executable
  *     reference interpreter (`formal/Exec.v`) produces the same
  *     observables.
  *
  * Output: `formal/validation/Generated.v` (git-ignored; regenerate with
  * `sbt "runMain esmeta.fv.FVExport"`). Compile it with `make validate`
  * in `formal/`. A mismatch anywhere fails that compilation — that is
  * the differential test.
  *
  * This file is part of the isolated `esmeta.fv` package and does not
  * change any existing ESMeta behavior.
  */
object FVExport {

  /** Interpreter that captures prints instead of writing to stdout, and
    * counts assertions ESMeta SILENTLY SKIPS.
    *
    * Interpreter.scala:147-151 wraps the asserted expression in
    * `optional(...)`, so an assertion whose evaluation throws is skipped
    * rather than failed ("skip not yet compiled assertions"). A program
    * containing such an assertion is not a usable differential test: it
    * "passes" in ESMeta without the assertion ever having been checked,
    * while the Rocq model — which does not swallow UB — gets stuck.
    * `exportFile` therefore refuses those programs with a reason instead
    * of emitting an expectation that would look like a model bug. */
  class CapturingInterpreter(st0: State)
    extends Interpreter(st0, timeLimit = Some(10)) {
    val prints: ListBuffer[Value] = ListBuffer()
    var skippedAsserts: Int = 0
    override def eval(inst: NormalInst): Unit = inst match
      case IPrint(expr) => prints += eval(expr)
      case IAssert(expr) =>
        esmeta.util.BaseUtils.optional(eval(expr)) match
          case None             => skippedAsserts += 1
          case Some(Bool(true)) =>
          case _                => throw esmeta.error.AssertionFail(expr)
      case _ => super.eval(inst)
  }

  /** thrown on out-of-fragment constructs */
  case class Unsupported(msg: String) extends Exception(msg)

  // ---------------------------------------------------------------------
  // translation to Rocq terms (mirrors formal/Fragment.v)
  // ---------------------------------------------------------------------

  def strLit(s: String): String =
    "\"" + s.replace("\"", "\"\"") + "\""

  def zLit(n: scala.math.BigInt): String =
    if (n < 0) s"($n)" else s"$n"

  /** ECMAScript strings are UTF-16 code units in the model (D-1).  ASCII
    * uses the readable [cu "..."] helper; anything else is emitted as an
    * explicit code-unit list so no character is silently mangled. */
  def cstrLit(s: String): String =
    if (s.forall(c => c >= ' ' && c < 128)) s"(cu ${strLit(s)})"
    else coqList(s.toCharArray.toList.map(c => c.toInt.toString))

  /** IEEE-754 double literal; 17 significant digits round-trip exactly. */
  def floatLit(d: Double): String =
    if (d.isNaN) "PrimFloat.nan"
    else if (d.isPosInfinity) "PrimFloat.infinity"
    else if (d.isNegInfinity) "PrimFloat.neg_infinity"
    else {
      val r = "%.17g".format(d)
      if (d < 0) s"(- $r)%float" else s"($r)%float"
    }

  def coqList(elems: List[String]): String =
    if (elems.isEmpty) "nil"
    else elems.mkString("(", " :: ", " :: nil)")

  def local(x: Local): String = x match
    case Name(name) => s"(LName ${strLit(name)})"
    case Temp(idx)  => s"(LTemp $idx)"

  def rocqRef(r: Ref): String = r match
    case x: Name          => s"(RVar (VLocal ${local(x)}))"
    case x: Temp          => s"(RVar (VLocal ${local(x)}))"
    case Global(name)     => s"(RVar (VGlobal ${strLit(name)}))"
    case Field(base, expr) => s"(RField ${rocqRef(base)} ${rocqExpr(expr)})"

  def rocqUOp(op: UOp): String = op match
    case UOp.Neg   => "UNeg"
    case UOp.Not   => "UNot"
    case UOp.Abs   => "UAbs"
    case UOp.Floor => "UFloor"
    case UOp.BNot  => "UBNot"
    case _         => throw Unsupported(s"uop: $op")

  def rocqBOp(op: BOp): String = op match
    case BOp.Add => "BAdd"
    case BOp.Sub => "BSub"
    case BOp.Mul => "BMul"
    case BOp.Lt  => "BLt"
    case BOp.Eq  => "BEq"
    case BOp.And   => "BAnd"
    case BOp.Or    => "BOr"
    case BOp.Div   => "BDiv"
    case BOp.Mod   => "BMod"
    case BOp.Equal  => "BEqual"
    case BOp.Pow    => "BPow"
    case BOp.BAnd   => "BBAnd"
    case BOp.BOr    => "BBOr"
    case BOp.BXOr   => "BBXOr"
    case BOp.LShift => "BLShift"
    case BOp.RShift => "BRShift"
    case _          => throw Unsupported(s"bop: $op")

  /** map an ESMeta type to the model's restricted [tyexp] (ADR-11).
    * Uses ESMeta's own stringification with a strict whitelist.
    *
    * ONLY types whose model test is EXACT are listed.  `Abrupt` and
    * `Normal` qualify since OQ-12: the model now checks the full field-map
    * refinement `ownFieldsOf` (TyModel.scala:86-93, RecordTy.scala:157-168,
    * manuals/types:27-39), and `completionRefinementStale` re-derives that
    * refinement from the live type model on every export so the
    * transcription cannot drift. */
  /** The model hard-codes the field-map refinements ESMeta uses for
    * `Abrupt` and `Normal` (Domain.v, OQ-12).  Re-derive them from the
    * live type model on every export: if they ever change, refuse to emit
    * `TAbrupt`/`TNormal` instead of silently mis-modelling 3355 type
    * tests.  Returns the reason it is stale, or None if it matches. */
  private lazy val completionRefinementStale: Option[String] = {
    val tm = esmeta.util.ManualInfo.tyModel
    def check(t: String, want: Map[String, String]): Option[String] = {
      val own = tm.ownFieldsOf(t)
      if (own.keySet != want.keySet)
        Some(s"$t has fields ${own.keySet}, model assumes ${want.keySet}")
      else
        own.collectFirst {
          case (f, b) if b.toString != want(f) =>
            s"$t.$f is '$b' but the model assumes '${want(f)}'"
        }
    }
    check(
      "AbruptCompletion",
      Map(
        "Type" -> ": Enum[~break~, ~continue~, ~return~, ~throw~]",
        "Value" -> ": ESValue | Enum[~empty~]",
        "Target" -> ": Enum[~empty~] | String",
      ),
    ).orElse(
      check(
        "NormalCompletion",
        Map(
          "Type" -> ": Enum[~normal~]",
          "Value" -> "",
          "Target" -> ": Enum[~empty~]",
        ),
      ),
    ).orElse(
      // the model relies on the third `RecordTy.contains` branch being the
      // one that decides, which needs this lca
      if (tm.lcaOf(("CompletionRecord", "AbruptCompletion")) !=
          Some("CompletionRecord"))
        Some("lcaOf(CompletionRecord, AbruptCompletion) is no longer " +
          "CompletionRecord")
      else None,
    )
  }

  private def completionTy(name: String, rocq: String): String =
    completionRefinementStale match
      case Some(why) => throw Unsupported(s"ty: $name (model stale: $why)")
      case None      => rocq

  /** Map an ESMeta type to the model's restricted [tyexp] (ADR-11).
    *
    * Decomposes the `ValueTy` STRUCTURALLY rather than matching its
    * stringification. `ValueTy` is a product of per-kind lattices and
    * `contains` dispatches on the value's kind (ValueTy.scala:167-188), so
    * a type with several non-bottom components is exactly a disjunction —
    * which is what `TUnion` models. Every component below is either
    * translated to a test the model performs exactly, or refused.
    *
    * An earlier version matched `t.ty.toString` against a whitelist. That
    * silently rotted when the stringifier changed: the keys said
    * `Record[CompletionRecord]` while the printer had moved to
    * `Completion`, so the most common type test in the specification was
    * rejected for months without anyone noticing. Structure does not rot
    * the same way — a new component is a compile error here, not a silent
    * miss. */
  def rocqTy(t: Type): String = t.ty match
    case _: UnknownTy => throw Unsupported(s"ty: unknown (${t.ty})")
    case vt: ValueTy =>
      val parts = scala.collection.mutable.ListBuffer[String]()
      def bad(what: String): Nothing = throw Unsupported(s"ty: ${t.ty}")

      // records
      vt.record match
        case RecordTy.Top => parts += "(TRecord \"Record\")"
        case RecordTy.Elem(m) if m.nonEmpty =>
          for ((name, fm) <- m) name match
            case "CompletionRecord" if fm.isTop => parts += "TCompletion"
            case "AbruptCompletion" if fm.isTop =>
              parts += completionTy("Abrupt", "TAbrupt")
            case "NormalCompletion" if fm.isTop =>
              parts += completionTy("Normal", "TNormal")
            // any other record type: only an unrefined name is exact,
            // because the model has no field-map refinement for it
            case n if fm.isTop && n.forall(c =>
                c.isLetterOrDigit || c == '.' || c == '_') =>
              parts += s"(TRecord ${strLit(n)})"
            case _ => bad("record")
        case _ => ()

      // lists and maps: only the unrefined forms; ListTy.Elem needs a
      // per-element heap lookup the model does not do yet
      vt.list match
        case ListTy.Top => parts += "TList"
        case ListTy.Bot => ()
        // ListTy.Elem: every element satisfies the element type.  The
        // model resolves one level of element addresses, so the element
        // type must itself need no resolution — no TAbrupt, no nested
        // TListOf.  Enforced here rather than assumed.
        case ListTy.Elem(elem) =>
          val inner = rocqTy(Type(elem))
          if (inner.contains("TAbrupt") || inner.contains("TListOf"))
            throw Unsupported(s"ty: ${t.ty} (element needs resolution)")
          parts += s"(TListOf $inner)"
        case _ => bad("list")
      vt.map match
        case MapTy.Top => parts += "TMapTy"
        case MapTy.Bot => ()
        case _         => bad("map")

      // ASTs (AstTy.scala:76-81)
      vt.ast match
        case AstTy.Top             => parts += "TAstTy"
        case AstTy.Simple(names) if names.nonEmpty =>
          parts += s"(TAstNames ${coqList(names.toList.sorted.map(strLit))})"
        case AstTy.Detail(n, i)    => parts += s"(TAstDetail ${strLit(n)} $i)"
        case _                     => ()

      // closures: only the top element, i.e. "any closure"
      if (!vt.clo.isBottom) {
        if (vt.clo.isTop) parts += "TCloTy" else bad("clo")
      }
      if (!vt.cont.isBottom) bad("cont")
      if (!vt.grammarSymbol.isBottom) bad("grammarSymbol")

      // primitives: only the unrefined forms.  A refined one (an enum with
      // a specific name set, a string with a specific value set, NumberInt,
      // ...) is a test the model does not perform.
      if (!vt.math.isBottom) {
        if (vt.math == MathTy.Top) parts += "TMathTy" else bad("math")
      }
      if (!vt.number.isBottom) {
        if (vt.number == NumberTy.Top) parts += "TNumberTy" else bad("number")
      }
      if (!vt.infinity.isBottom) {
        if (vt.infinity.isTop) parts += "TInfinityTy" else bad("infinity")
      }
      if (!vt.enumv.isBottom) {
        if (vt.enumv.isTop) parts += "TEnumTy" else bad("enum")
      }
      if (!vt.str.isBottom) {
        if (vt.str.isTop) parts += "TStrTy" else bad("str")
      }
      if (!vt.bool.isBottom) {
        if (vt.bool.isTop) parts += "TBoolTy" else bad("bool")
      }
      if (vt.codeUnit) parts += "TCodeUnitTy"
      if (vt.bigInt) parts += "TBigIntTy"
      if (vt.undef) parts += "TUndefTy"
      if (vt.nullv) parts += "TNullTy"

      parts.toList match
        case Nil      => throw Unsupported(s"ty: ${t.ty} (empty)")
        case p :: Nil => p
        case ps       => s"(TUnion ${coqList(ps)})"

  def rocqExpr(e: Expr): String = e match
    case EMath(n) =>
      if (!n.isWhole) throw Unsupported(s"non-integer Math literal: $n")
      s"(EMath ${zLit(n.toBigInt)})"
    case EBool(b)         => s"(EBool $b)"
    case EStr(s)          => s"(EStr ${cstrLit(s)})"
    case EUndef()         => "EUndef"
    case ENull()          => "ENull"
    case EEnum(name)      => s"(EEnum ${strLit(name)})"
    case ERef(ref)        => s"(ERef ${rocqRef(ref)})"
    case EUnary(uop, e1)  => s"(EUnary ${rocqUOp(uop)} ${rocqExpr(e1)})"
    case EBinary(bop, l, r) =>
      s"(EBinary ${rocqBOp(bop)} ${rocqExpr(l)} ${rocqExpr(r)})"
    case EClo(fname, captured) =>
      s"(EClo ${strLit(fname)} ${coqList(captured.map(x => strLit(x.name)))})"
    case EList(exprs)  => s"(EList ${coqList(exprs.map(rocqExpr))})"
    case ESizeOf(e1)   => s"(ESizeOf ${rocqExpr(e1)})"
    case ERecord(tname, pairs) =>
      val fields = pairs.map { case (f, e) => s"(${strLit(f)}, ${rocqExpr(e)})" }
      s"(ERecord ${strLit(tname)} ${coqList(fields)})"
    case EExists(ref)          => s"(EExists ${rocqRef(ref)})"
    case ETypeOf(base)         => s"(ETypeOf ${rocqExpr(base)})"
    case ETypeCheck(base, ty)  => s"(ETypeCheck ${rocqExpr(base)} ${rocqTy(ty)})"
    case EYet(msg)             => s"(EYet ${strLit(msg)})"
    case EMap(_, pairs) =>
      val ps = pairs.map { case (k, v) => s"(${rocqExpr(k)}, ${rocqExpr(v)})" }
      s"(EMap ${coqList(ps)})"
    case EKeys(m, intSorted)   => s"(EKeys ${rocqExpr(m)} $intSorted)"
    case ECopy(obj)            => s"(ECopy ${rocqExpr(obj)})"
    case ENumber(d)            => s"(ENumber ${floatLit(d)})"
    case EBigInt(n)            => s"(EBigInt ${zLit(n)})"
    case EInfinity(pos)        => s"(EInfinity $pos)"
    case ECodeUnit(c)          => s"(ECodeUnit ${c.toInt})"
    case EConvert(cop, e1) =>
      val op = cop match
        case COp.ToApproxNumber => "CToApproxNumber"
        case COp.ToNumber       => "CToNumber"
        case COp.ToBigInt       => "CToBigInt"
        case COp.ToMath         => "CToMath"
        case COp.ToCodeUnit     => "CToCodeUnit"
        case COp.ToStr(_)       => throw Unsupported("cop: ToStr")
      s"(EConvert $op ${rocqExpr(e1)})"
    case EVariadic(vop, exprs) =>
      val op = vop match
        case VOp.Min    => "VoMin"
        case VOp.Max    => "VoMax"
        case VOp.Concat => "VoConcat"
      s"(EVariadic $op ${coqList(exprs.map(rocqExpr))})"
    case EContains(list, expr) =>
      s"(EContains ${rocqExpr(list)} ${rocqExpr(expr)})"
    case EGrammarSymbol(name, params) =>
      s"(EGrammarSymbol ${strLit(name)} ${coqList(params.map(_.toString))})"
    case EInstanceOf(expr, target) =>
      s"(EInstanceOf ${rocqExpr(expr)} ${rocqExpr(target)})"
    case ESubstring(expr, from, to) =>
      val toTerm = to.fold("None")(e => s"(Some ${rocqExpr(e)})")
      s"(ESubstring ${rocqExpr(expr)} ${rocqExpr(from)} $toTerm)"
    case ESourceText(expr) => s"(ESourceText ${rocqExpr(expr)})"
    // only the cached-AST fast path is modelled; a real parse is UB
    case EParse(code, rule) => s"(EParse ${rocqExpr(code)} ${rocqExpr(rule)})"
    case _             => throw Unsupported(s"expr: ${e.getClass.getSimpleName}")

  def rocqInst(i: Inst): String = i match
    case INop()          => "INop"
    case ISeq(insts)     => s"(ISeq ${coqList(insts.map(rocqInst))})"
    case IExpr(e)        => s"(IExpr ${rocqExpr(e)})"
    case ILet(lhs, e)    => s"(ILet ${strLit(lhs.name)} ${rocqExpr(e)})"
    case IAssign(r, e)   => s"(IAssign ${rocqRef(r)} ${rocqExpr(e)})"
    case IIf(c, t, e, _) => s"(IIf ${rocqExpr(c)} ${rocqInst(t)} ${rocqInst(e)})"
    case IWhile(c, body) => s"(IWhile ${rocqExpr(c)} ${rocqInst(body)})"
    case ICall(lhs, f, args) =>
      s"(ICall ${local(lhs)} ${rocqExpr(f)} ${coqList(args.map(rocqExpr))})"
    case IReturn(e) => s"(IReturn ${rocqExpr(e)})"
    case IAssert(e) => s"(IAssert ${rocqExpr(e)})"
    case IPrint(e)  => s"(IPrint ${rocqExpr(e)})"
    case IPush(elem, list, front) =>
      s"(IPush ${rocqExpr(elem)} ${rocqExpr(list)} $front)"
    case IPop(lhs, list, front) =>
      s"(IPop ${local(lhs)} ${rocqExpr(list)} $front)"
    case IExpand(base, expr) => s"(IExpand ${rocqRef(base)} ${rocqExpr(expr)})"
    case IDelete(base, expr) => s"(IDelete ${rocqRef(base)} ${rocqExpr(expr)})"
    case ISdoCall(lhs, base, method, args) =>
      s"(ISdoCall ${local(lhs)} ${rocqExpr(base)} ${strLit(method)} " +
      s"${coqList(args.map(rocqExpr))})"
    case _ => throw Unsupported(s"inst: ${i.getClass.getSimpleName}")

  def rocqFunc(f: Func): String = {
    // The optional flag is dropped along with the parameter types (L-3).
    // `getLocals` binds positionally and, when the arguments run out,
    // leaves the remaining parameters unbound whether or not they are
    // optional (Interpreter.scala:377-381 — the non-optional branch builds
    // a `RemainingParams` it never throws).  `init_env` mirrors that, so
    // the flag carries no semantics the model needs.
    val params = f.params.map(p => strLit(p.lhs.name))
    s"mkFunc ${f.main} ${strLit(f.name)} ${coqList(params)} ${rocqInst(f.body)}"
  }

  /** observable values (address-free per the observable-behavior spec) */
  def rocqValue(v: Value): String = v match
    case Math(d) =>
      if (!d.isWhole) throw Unsupported(s"non-integer Math value: $d")
      s"(VMath ${zLit(d.toBigInt)})"
    case Bool(b)    => s"(VBool $b)"
    case Str(s)     => s"(VStr ${cstrLit(s)})"
    case Undef      => "VUndef"
    case Null       => "VNull"
    case Enum(name) => s"(VEnum ${strLit(name)})"
    case Number(d)  => s"(VNumber ${floatLit(d)})"
    case CodeUnit(c) => s"(VCodeUnit ${c.toInt})"
    case Infinity(p) => s"(VInfinity $p)"
    case BigInt(n)   => s"(VBigInt ${zLit(n)})"
    case GrammarSymbol(name, params) =>
      s"(VGrammarSymbol ${strLit(name)} ${coqList(params.map(_.toString))})"
    case _ => throw Unsupported(s"observable value: ${v.getClass.getSimpleName}")

  // ---------------------------------------------------------------------
  // per-file export
  // ---------------------------------------------------------------------

  val FUEL = 1_000_000

  case class Exported(id: String, source: String, defs: String)
  case class Skipped(source: String, reason: String)

  def sanitize(name: String): String =
    "g_" + name.replaceAll("[^A-Za-z0-9_]", "_")

  def exportFile(path: String): Either[Skipped, Exported] =
    Try {
      val program = Program.fromFile(path)
      val funcs = program.funcs
      val id = sanitize(
        path.stripSuffix(".ir").split("/").takeRight(2).mkString("_"),
      )

      // translate first: rejects out-of-fragment constructs
      val funcDefs = funcs.zipWithIndex.map {
        case (f, i) => (s"${id}_f$i", rocqFunc(f))
      }

      // run with ESMeta's interpreter, capturing observables
      val st = State(CFGBuilder(program))
      val interp = new CapturingInterpreter(st)
      val finalSt = interp.result
      if (interp.skippedAsserts > 0)
        throw Unsupported(
          s"ESMeta silently skipped ${interp.skippedAsserts} assertion(s) " +
          "(Interpreter.scala:147-151); not a valid differential test",
        )
      val result = finalSt.globals.getOrElse(GLOBAL_RESULT, Undef)
      val resultTerm = rocqValue(result)
      val printTerms = interp.prints.toList.map(rocqValue)

      val sb = new StringBuilder
      sb ++= s"(* ${path.stripPrefix(s"$BASE_DIR/")} *)\n"
      for ((name, body) <- funcDefs)
        sb ++= s"Definition $name : func :=\n  $body.\n"
      sb ++= s"Definition $id : prog := mkProg ${coqList(funcDefs.map(_._1))}.\n"
      sb ++= s"Example ${id}_ok : run $FUEL $id = " +
        s"Ok ($resultTerm, ${coqList(printTerms)}).\n"
      sb ++= "Proof. vm_compute. reflexivity. Qed.\n"
      Exported(id, path, sb.toString)
    } match
      case Success(e)                => Right(e)
      case Failure(Unsupported(msg)) => Left(Skipped(path, msg))
      case Failure(err)              => Left(Skipped(path, s"run failed: $err"))

  // ---------------------------------------------------------------------
  // entry point
  // ---------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    val inputs =
      if (args.nonEmpty) args.toList
      else
        // ESMeta's own IR corpus, plus targeted differential cases added
        // for constructs the corpus does not reach (formal/validation/extra)
        List(s"$BASE_DIR/tests/ir", s"$BASE_DIR/formal/validation/extra")
          .flatMap(walkTree)
          .filter(f => irFilter(f.getName))
          .map(_.toString)
          .sorted

    val results = inputs.map(exportFile)
    val exported = results.collect { case Right(e) => e }
    val skipped = results.collect { case Left(s) => s }

    val header =
      s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVExport"` — DO NOT EDIT.
         | *
         | * Differential validation (PO-011, testing NOT proof): for each
         | * fragment-compatible standalone IR program, the expectation below
         | * records the observables of ESMeta's interpreter (final RESULT
         | * global + IPrint sequence); compiling this file checks that the
         | * Rocq executable reference interpreter agrees.
         | *
         | * Exported: ${exported.size}, skipped: ${skipped.size}
         | *)
         |From Stdlib Require Import String ZArith List Floats.
         |Import ListNotations.
         |From ESMetaFV Require Import Fragment Domain Exec.
         |Local Open Scope string_scope.
         |Local Open Scope Z_scope.
         |""".stripMargin

    val body = exported.map(_.defs).mkString("\n")
    val outPath = s"$BASE_DIR/formal/validation/Generated.v"
    dumpFile(header + "\n" + body, outPath)

    println(s"[fv] exported ${exported.size} program(s) to $outPath")
    if (skipped.nonEmpty) {
      println(s"[fv] skipped ${skipped.size} file(s):")
      for (Skipped(src, reason) <- skipped)
        println(s"  - ${src.stripPrefix(s"$BASE_DIR/")}: $reason")
    }
  }

  private def irFilter(name: String): Boolean = name.endsWith(".ir")

  private def walkTree(dirname: String): Seq[File] = {
    def walk(file: File): Seq[File] =
      if (file.isDirectory) file.listFiles.toSeq.sortBy(_.getName).flatMap(walk)
      else Seq(file)
    walk(new File(dirname))
  }
}
