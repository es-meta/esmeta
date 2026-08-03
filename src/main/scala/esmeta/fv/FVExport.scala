package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.spec.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.{Fin, Inf}
import esmeta.util.SystemUtils.*
import java.io.File
import java.util.Locale
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
  *     reference interpreter (`formal/Exec.v`) produces the same observables.
  *
  * Output: `formal/validation/Generated.v` (git-ignored; regenerate with `sbt
  * "runMain esmeta.fv.FVExport"`). Compile it with `make validate` in
  * `formal/`. A mismatch anywhere fails that compilation — that is the
  * differential test.
  *
  * This file is part of the isolated `esmeta.fv` package and does not change
  * any existing ESMeta behavior.
  */
object FVExport {

  /** Interpreter that captures prints instead of writing to stdout, and
    * classifies assertions ESMeta SILENTLY SKIPS.
    *
    * Interpreter.scala:147-151 wraps the asserted expression in
    * `optional(...)`, so an assertion whose evaluation throws is skipped rather
    * than failed ("skip not yet compiled assertions"). Bare `IAssert(EYet(_))`
    * is an intentional marker for a formal-semantics assertion that has not
    * been compiled and is deliberately a no-op. Exec.v and the ITree semantics
    * model that exact bare form as a no-op. Any other evaluation failure is
    * unsafe to treat that way, so both exporters refuse every other skipped
    * assertion.
    */
  class CapturingInterpreter(st0: State)
    extends Interpreter(st0, timeLimit = Some(10)) {
    val prints: ListBuffer[Value] = ListBuffer()
    var skippedYetAsserts: Int = 0
    var failedToEvaluateAsserts: Int = 0
    override def eval(inst: NormalInst): Unit = inst match
      case IPrint(expr) => prints += eval(expr)
      case IAssert(expr) =>
        esmeta.util.BaseUtils.optional(eval(expr)) match
          case None =>
            expr match
              case EYet(_) => skippedYetAsserts += 1
              case _       => failedToEvaluateAsserts += 1
          case Some(Bool(true)) =>
          case _                => throw esmeta.error.AssertionFail(expr)
      case _ => super.eval(inst)
  }

  /** thrown on out-of-fragment constructs */
  case class Unsupported(msg: String) extends Exception(msg)

  /** Downgrade only a declared model-boundary rejection. Parser, CFG, exporter,
    * and runtime defects must remain failures rather than silently reducing
    * formal coverage.
    */
  private[fv] def unsupportedToNone[A](body: => A): Option[A] =
    try Some(body)
    catch case _: Unsupported => None

  // ---------------------------------------------------------------------
  // translation to Rocq terms (mirrors formal/Fragment.v)
  // ---------------------------------------------------------------------

  def strLit(s: String): String =
    "\"" + s.replace("\"", "\"\"") + "\""

  def zLit(n: scala.math.BigInt): String =
    if (n < 0) s"($n)" else s"$n"

  /** Keep generated naturals compact through Rocq extraction. Ordinary numerals
    * elaborate to unary [S] chains even when OCaml extraction maps [nat] to
    * [int].
    */
  def natLit(n: Int): String =
    s"""(nat_decimal "$n"%pstring)"""

  /** ECMAScript strings are UTF-16 code units in the model (D-1). ASCII uses
    * the readable [cu "..."] helper; anything else is emitted as an explicit
    * code-unit list so no character is silently mangled.
    */
  def cstrLit(s: String): String =
    if (s.forall(c => c >= ' ' && c < 128)) s"(cu ${strLit(s)})"
    else coqList(s.toCharArray.toList.map(c => c.toInt.toString))

  /** IEEE-754 double literal; 17 significant digits round-trip exactly. */
  def floatLit(d: Double): String =
    if (d.isNaN) "PrimFloat.nan"
    else if (d.isPosInfinity) "PrimFloat.infinity"
    else if (d.isNegInfinity) "PrimFloat.neg_infinity"
    else {
      val r = String.format(Locale.ROOT, "%.17g", Double.box(d))
      // `r` already contains the sign.  Prefixing another Rocq unary
      // minus turned every finite negative value except -0.0 positive
      // (for example -1.0 became `(- -1.0)%float`).
      s"($r)%float"
    }

  def coqList(elems: List[String]): String =
    if (elems.isEmpty) "nil"
    else elems.mkString("(", " :: ", " :: nil)")

  def local(x: Local): String = x match
    case Name(name) => s"(LName ${strLit(name)})"
    case Temp(idx)  => s"(LTemp ${natLit(idx)})"

  def rocqRef(r: Ref)(using CFG): String = r match
    case x: Name           => s"(RVar (VLocal ${local(x)}))"
    case x: Temp           => s"(RVar (VLocal ${local(x)}))"
    case Global(name)      => s"(RVar (VGlobal ${strLit(name)}))"
    case Field(base, expr) => s"(RField ${rocqRef(base)} ${rocqExpr(expr)})"

  /** Operand fragment for which the Rocq model keeps ESMeta evaluator
    * exceptions distinct from model/cache failure until EParse catches them.
    * Keep this predicate synchronized with Semantics.denote_parse_operand and
    * Exec.exec_parse_operand.
    */
  private[fv] def parseOperandSupported(e: Expr): Boolean = e match
    case _: LiteralExpr       => true
    case EGrammarSymbol(_, _) => true
    case EYet(_)              => true
    case ERef(ref)            => parseRefSupported(ref)
    case EList(exprs)         => exprs.forall(parseOperandSupported)
    case ESourceText(inner)   => parseOperandSupported(inner)
    case _                    => false

  private[fv] def parseRefSupported(ref: Ref): Boolean = ref match
    case _: Var => true
    case Field(base, key) =>
      parseRefSupported(base) && parseOperandSupported(key)

  def rocqUOp(op: UOp): String = op match
    case UOp.Neg   => "UNeg"
    case UOp.Not   => "UNot"
    case UOp.Abs   => "UAbs"
    case UOp.Floor => "UFloor"
    case UOp.BNot  => "UBNot"
    case _         => throw Unsupported(s"uop: $op")

  def rocqBOp(op: BOp): String = op match
    case BOp.Add    => "BAdd"
    case BOp.Sub    => "BSub"
    case BOp.Mul    => "BMul"
    case BOp.Lt     => "BLt"
    case BOp.Eq     => "BEq"
    case BOp.And    => "BAnd"
    case BOp.Or     => "BOr"
    case BOp.Div    => "BDiv"
    case BOp.Mod    => "BMod"
    case BOp.Equal  => "BEqual"
    case BOp.Pow    => "BPow"
    case BOp.BAnd   => "BBAnd"
    case BOp.BOr    => "BBOr"
    case BOp.BXOr   => "BBXOr"
    case BOp.LShift => "BLShift"
    case BOp.RShift => "BRShift"
    case _          => throw Unsupported(s"bop: $op")

  /** Mathematical operators are evaluated by ESMeta's Scala host. The Rocq
    * model preserves the IR node and replays only an exact, typed result from
    * the per-program host cache; it does not approximate these functions.
    */
  def rocqMOp(op: MOp): String = op match
    case MOp.Expm1 => "MExpm1"
    case MOp.Log10 => "MLog10"
    case MOp.Log2  => "MLog2"
    case MOp.Cos   => "MCos"
    case MOp.Cbrt  => "MCbrt"
    case MOp.Exp   => "MExp"
    case MOp.Cosh  => "MCosh"
    case MOp.Sinh  => "MSinh"
    case MOp.Tanh  => "MTanh"
    case MOp.Acos  => "MAcos"
    case MOp.Acosh => "MAcosh"
    case MOp.Asinh => "MAsinh"
    case MOp.Atanh => "MAtanh"
    case MOp.Asin  => "MAsin"
    case MOp.Atan2 => "MAtan2"
    case MOp.Atan  => "MAtan"
    case MOp.Log1p => "MLog1p"
    case MOp.Log   => "MLog"
    case MOp.Sin   => "MSin"
    case MOp.Sqrt  => "MSqrt"
    case MOp.Tan   => "MTan"

  /** map an ESMeta type to the model's restricted [tyexp] (ADR-11). Uses
    * ESMeta's own stringification with a strict whitelist.
    *
    * ONLY types whose model test is EXACT are listed. `Abrupt` and `Normal`
    * qualify since OQ-12: the model now checks the full field-map refinement
    * `ownFieldsOf` (TyModel.scala:86-93, RecordTy.scala:157-168,
    * manuals/types:27-39), and `completionRefinementStale` re-derives that
    * refinement from the live type model on every export so the transcription
    * cannot drift.
    */
  /** The model hard-codes the field-map refinements ESMeta uses for `Abrupt`
    * and `Normal` (Domain.v, OQ-12). Re-derive them from the live type model on
    * every export: if they ever change, refuse to emit `TAbrupt`/`TNormal`
    * instead of silently mis-modelling 3355 type tests. Returns the reason it
    * is stale, or None if it matches.
    */
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
      if (
        tm.lcaOf(("CompletionRecord", "AbruptCompletion")) !=
        Some("CompletionRecord")
      )
        Some(
          "lcaOf(CompletionRecord, AbruptCompletion) is no longer " +
          "CompletionRecord",
        )
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
    * `contains` dispatches on the value's kind (ValueTy.scala:167-188), so a
    * type with several non-bottom components is exactly a disjunction — which
    * is what `TUnion` models. Every component below is either translated to a
    * test the model performs exactly, or refused.
    *
    * An earlier version matched `t.ty.toString` against a whitelist. That
    * silently rotted when the stringifier changed: the keys said
    * `Record[CompletionRecord]` while the printer had moved to `Completion`, so
    * the most common type test in the specification was rejected for months
    * without anyone noticing. Structure does not rot the same way — a new
    * component is a compile error here, not a silent miss.
    */
  def rocqTy(t: Type): String = t.ty match
    case _: UnknownTy => throw Unsupported(s"ty: unknown (${t.ty})")
    case vt: ValueTy =>
      val parts = scala.collection.mutable.ListBuffer[String]()
      def bad(what: String): Nothing =
        throw Unsupported(s"ty: $what: ${t.ty}")

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
            // A field map made entirely of Binding.Exist requires exactly
            // field presence; its values are otherwise unconstrained.  Keep
            // the record name (including the anonymous empty name) so Rocq's
            // RecordTy.contains implementation applies the same nominal and
            // structural branches as ESMeta.
            case n
                if fm.map.nonEmpty &&
                fm.map.values.forall(_ == Binding.Exist) =>
              val fields = fm.map.keys.toList.sorted.map(strLit)
              parts +=
                s"(TRecordFields ${strLit(n)} ${coqList(fields)})"
            // any other record type: only an unrefined name is exact,
            // because the model has no field-map refinement for it
            case n
                if fm.isTop && n
                  .forall(c => c.isLetterOrDigit || c == '.' || c == '_') =>
              parts += s"(TRecord ${strLit(n)})"
            case _ => bad("record")
        case _ => ()

      // Lists preserve element refinements.  The Rocq checker is recursive,
      // but the exporter still conservatively refuses TAbrupt and nested
      // TListOf until those two source-to-model translations are audited.
      // Maps remain unrefined.
      vt.list match
        case ListTy.Top => parts += "TList"
        case ListTy.Bot => ()
        // ListTy.Elem: every element satisfies the element type.  The gate
        // below is an exporter support boundary, not a limitation of the
        // recursive lazy heap checker.
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
        case AstTy.Top => parts += "TAstTy"
        case AstTy.Simple(names) if names.nonEmpty =>
          parts += s"(TAstNames ${coqList(names.toList.sorted.map(strLit))})"
        case AstTy.Detail(n, i) =>
          parts += s"(TAstDetail ${strLit(n)} ${natLit(i)})"
        case _ => ()

      // closures: only the top element, i.e. "any closure"
      if (!vt.clo.isBottom) {
        if (vt.clo.isTop) parts += "TCloTy" else bad("clo")
      }
      if (!vt.cont.isBottom) bad("cont")
      if (!vt.grammarSymbol.isBottom) bad("grammarSymbol")

      // Supported primitive refinements are mapped exactly; every remaining
      // form fails closed.  In particular, finite Number sets stay unsupported.
      if (!vt.math.isBottom) {
        vt.math match
          case MathTy.Top => parts += "TMathTy"
          case MathIntTy(IntSignTy(sign)) =>
            parts += s"(TMathInt ${sign.neg} ${sign.zero} ${sign.pos})"
          case MathIntTy(IntSetTy(set)) =>
            parts +=
              s"(TMathIntSet ${coqList(set.toList.sorted.map(zLit))})"
          case _ => bad("math")
      }
      if (!vt.number.isBottom) {
        vt.number match
          case NumberTy.Top => parts += "TNumberTy"
          case NumberIntTy(IntSignTy(sign), hasNaN) =>
            parts +=
              s"(TNumberInt ${sign.neg} ${sign.zero} ${sign.pos} $hasNaN)"
          case _ => bad("number")
      }
      if (!vt.infinity.isBottom) {
        if (vt.infinity.isTop) parts += "TInfinityTy"
        else {
          val allowNeg = vt.infinity.pos.contains(false)
          val allowPos = vt.infinity.pos.contains(true)
          parts += s"(TInfinity $allowNeg $allowPos)"
        }
      }
      if (!vt.enumv.isBottom) {
        vt.enumv match
          case Inf => parts += "TEnumTy"
          case Fin(names) if names.nonEmpty =>
            parts +=
              s"(TEnumNames ${coqList(names.toList.sorted.map(strLit))})"
          case _ => bad("enum")
      }
      if (!vt.str.isBottom) {
        vt.str match
          case Inf => parts += "TStrTy"
          case Fin(set) if set.nonEmpty =>
            parts +=
              s"(TStrSet ${coqList(set.toList.sorted.map(cstrLit))})"
          case _ => bad("str")
      }
      if (!vt.bool.isBottom) {
        if (vt.bool.isTop) parts += "TBoolTy"
        else {
          val allowFalse = vt.bool.set.contains(false)
          val allowTrue = vt.bool.set.contains(true)
          parts += s"(TBoolSet $allowFalse $allowTrue)"
        }
      }
      if (vt.codeUnit) parts += "TCodeUnitTy"
      if (vt.bigInt) parts += "TBigIntTy"
      if (vt.undef) parts += "TUndefTy"
      if (vt.nullv) parts += "TNullTy"

      parts.toList match
        case Nil      => throw Unsupported(s"ty: ${t.ty} (empty)")
        case p :: Nil => p
        case ps       => s"(TUnion ${coqList(ps)})"

  def rocqFuncKind(kind: FuncKind): String = kind match
    case FuncKind.AbsOp        => "FKAbsOp"
    case FuncKind.NumMeth      => "FKNumMeth"
    case FuncKind.SynDirOp     => "FKSynDirOp"
    case FuncKind.ConcMeth     => "FKConcMeth"
    case FuncKind.InternalMeth => "FKInternalMeth"
    case FuncKind.Builtin      => "FKBuiltin"
    case FuncKind.Clo          => "FKClo"
    case FuncKind.Cont         => "FKCont"
    case FuncKind.Aux          => "FKAux"

  /** Preserve every source annotation, even when its full type lies outside the
    * executable [tyexp] subset. [None] is a proof-visible boundary, not an
    * approximation to Any; exact supported annotations additionally carry the
    * Rocq checker used by type-based lemmas.
    */
  def rocqTypeAnnotation(t: Type): String = {
    val checker = unsupportedToNone(rocqTy(t)) match
      case Some(ty) => s"(Some $ty)"
      case None     => "None"
    s"(mkTypeAnnotation ${strLit(t.ty.toString)} $checker)"
  }

  def rocqParamAnnotation(p: esmeta.ir.Param): String =
    s"(mkParamAnnotation ${rocqTypeAnnotation(p.ty)} ${p.optional})"

  def rocqExpr(e: Expr)(using cfg: CFG): String = e match
    case EMath(n) =>
      if (!n.isWhole) throw Unsupported(s"non-integer Math literal: $n")
      s"(EMath ${zLit(n.toBigInt)})"
    case EBool(b)        => s"(EBool $b)"
    case EStr(s)         => s"(EStr ${cstrLit(s)})"
    case EUndef()        => "EUndef"
    case ENull()         => "ENull"
    case EEnum(name)     => s"(EEnum ${strLit(name)})"
    case ERef(ref)       => s"(ERef ${rocqRef(ref)})"
    case EUnary(uop, e1) => s"(EUnary ${rocqUOp(uop)} ${rocqExpr(e1)})"
    case EBinary(bop, l, r) =>
      s"(EBinary ${rocqBOp(bop)} ${rocqExpr(l)} ${rocqExpr(r)})"
    case EMathOp(mop, args) =>
      s"(EMathOp ${rocqMOp(mop)} ${coqList(args.map(rocqExpr))})"
    case EClo(fname, captured) =>
      s"(EClo ${strLit(fname)} ${coqList(captured.map(x => strLit(x.name)))})"
    case ECont(fname) => s"(ECont ${strLit(fname)})"
    case EList(exprs) => s"(EList ${coqList(exprs.map(rocqExpr))})"
    case ESizeOf(e1)  => s"(ESizeOf ${rocqExpr(e1)})"
    case ERecord(tname, pairs) =>
      val fields = pairs.map {
        case (f, e) => s"(${strLit(f)}, ${rocqExpr(e)})"
      }
      s"(ERecord ${strLit(tname)} ${coqList(fields)})"
    case EExists(ref)         => s"(EExists ${rocqRef(ref)})"
    case ETypeOf(base)        => s"(ETypeOf ${rocqExpr(base)})"
    case ETypeCheck(base, ty) => s"(ETypeCheck ${rocqExpr(base)} ${rocqTy(ty)})"
    case EYet(msg)            => s"(EYet ${strLit(msg)})"
    case EMap(_, pairs) =>
      val ps = pairs.map { case (k, v) => s"(${rocqExpr(k)}, ${rocqExpr(v)})" }
      s"(EMap ${coqList(ps)})"
    case EKeys(m, intSorted) => s"(EKeys ${rocqExpr(m)} $intSorted)"
    case ECopy(obj)          => s"(ECopy ${rocqExpr(obj)})"
    case ENumber(d)          => s"(ENumber ${floatLit(d)})"
    case EBigInt(n)          => s"(EBigInt ${zLit(n)})"
    case EInfinity(pos)      => s"(EInfinity $pos)"
    case ECodeUnit(c)        => s"(ECodeUnit ${c.toInt})"
    case EConvert(cop, e1) =>
      cop match
        case COp.ToStr(radix) =>
          val radixTerm =
            radix.fold("None")(e => s"(Some ${rocqExpr(e)})")
          s"(EToStr ${rocqExpr(e1)} $radixTerm)"
        case _ =>
          val op = cop match
            case COp.ToApproxNumber => "CToApproxNumber"
            case COp.ToNumber       => "CToNumber"
            case COp.ToBigInt       => "CToBigInt"
            case COp.ToMath         => "CToMath"
            case COp.ToCodeUnit     => "CToCodeUnit"
            case COp.ToStr(_)       => throw IllegalStateException()
          s"(EConvert $op ${rocqExpr(e1)})"
    case EVariadic(vop, exprs) =>
      val op = vop match
        case VOp.Min    => "VoMin"
        case VOp.Max    => "VoMax"
        case VOp.Concat => "VoConcat"
      s"(EVariadic $op ${coqList(exprs.map(rocqExpr))})"
    case EContains(list, expr) =>
      s"(EContains ${rocqExpr(list)} ${rocqExpr(expr)})"
    case ETrim(expr, isStarting) =>
      s"(ETrim ${rocqExpr(expr)} $isStarting)"
    case ESyntactic(name, args, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap
        .getOrElse(
          name,
          throw Unsupported(s"expr: ESyntactic (unknown production $name)"),
        )
        .rhsVec
        .lift(rhsIdx)
        .getOrElse(
          throw Unsupported(
            s"expr: ESyntactic (invalid RHS $name[$rhsIdx])",
          ),
        )
      if (children.length != rhs.nts.length)
        throw Unsupported(
          s"expr: ESyntactic ($name[$rhsIdx] has ${children.length} " +
          s"children, grammar expects ${rhs.nts.length})",
        )

      // Ast.subIdx is exactly the bit-vector of present optional
      // nonterminals, with the rightmost optional as the least-significant
      // bit (Ast.scala:116-128).
      val optionalPresent =
        (rhs.ntsWithOptional zip children).collect {
          case ((_, true), child) => child.nonEmpty
        }
      val subIdx = optionalPresent.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
      val childNames = rhs.nts.map(nt => strLit(nt.name))

      // The Coq model has no grammar.  Preserve the exact RHS source
      // printer as a compact layout: terminals carry their UTF-16 text;
      // every nonterminal is a `None` slot consuming one runtime child.
      val sourceLayout = rhs.symbols.flatMap {
        case Terminal(term) => Some(s"(Some ${cstrLit(term)})")
        case symbol if symbol.getNt.isDefined => Some("None")
        case _                                => None
      }
      val childTerms = children.toList.map {
        case Some(child) => s"(Some ${rocqExpr(child)})"
        case None        => "None"
      }
      s"(ESyntactic ${strLit(name)} " +
      s"${coqList(args.map(_.toString))} ${natLit(rhsIdx)} " +
      s"${natLit(subIdx)} ${coqList(childTerms)} " +
      s"${coqList(childNames)} ${coqList(sourceLayout)})"
    case EGrammarSymbol(name, params) =>
      s"(EGrammarSymbol ${strLit(name)} ${coqList(params.map(_.toString))})"
    case EInstanceOf(expr, target) =>
      s"(EInstanceOf ${rocqExpr(expr)} ${rocqExpr(target)})"
    case ESubstring(expr, from, to) =>
      val toTerm = to.fold("None")(e => s"(Some ${rocqExpr(e)})")
      s"(ESubstring ${rocqExpr(expr)} ${rocqExpr(from)} $toTerm)"
    case ESourceText(expr) => s"(ESourceText ${rocqExpr(expr)})"
    case EParse(code, rule) =>
      if (!parseOperandSupported(code))
        throw Unsupported(
          s"expr: EParse unsupported code operand ${code.getClass.getSimpleName}",
        )
      if (!parseOperandSupported(rule))
        throw Unsupported(
          s"expr: EParse unsupported rule operand ${rule.getClass.getSimpleName}",
        )
      s"(EParse ${rocqExpr(code)} ${rocqExpr(rule)})"
    case _ => throw Unsupported(s"expr: ${e.getClass.getSimpleName}")

  def rocqInst(i: Inst)(using CFG): String = i match
    case INop()        => "INop"
    case ISeq(insts)   => s"(ISeq ${coqList(insts.map(rocqInst))})"
    case IExpr(e)      => s"(IExpr ${rocqExpr(e)})"
    case ILet(lhs, e)  => s"(ILet ${strLit(lhs.name)} ${rocqExpr(e)})"
    case IAssign(r, e) => s"(IAssign ${rocqRef(r)} ${rocqExpr(e)})"
    case IIf(c, t, e, _) =>
      s"(IIf ${rocqExpr(c)} ${rocqInst(t)} ${rocqInst(e)})"
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

  /** PropertyDescriptor records have a closed, specification-defined field set.
    * The generated IR for ValidateAndApplyPropertyDescriptor uses EKeys(Desc,
    * false) for (1) an emptiness test and (2) copying every present descriptor
    * field. RecordObj is backed by a generic HashMap, so exporting those
    * operations would make the formal model depend on an iteration order that
    * is neither specified nor needed by the algorithm.
    *
    * Normalize the two source-level patterns structurally:
    *   - the emptiness test becomes six fixed EExists checks;
    *   - the copy loop becomes six guarded assignments.
    *
    * Compilation duplicates the copy loop into four control-flow branches,
    * hence the current IR contains five EKeys nodes in total. Exact shape and
    * occurrence checks below deliberately fail closed if the compiler or
    * specification changes; this must never silently become a partial copy.
    */
  private[fv] val propertyDescriptorFields: List[String] =
    List(
      "Value",
      "Writable",
      "Get",
      "Set",
      "Enumerable",
      "Configurable",
    )

  private def descRef: Expr = ERef(Name("Desc"))
  private def descField(name: String): Field =
    Field(Name("Desc"), EStr(name))
  private def localRef(name: String): Expr = ERef(Name(name))

  private def descriptorEmptyTest: Expr =
    propertyDescriptorFields
      .map(name => EUnary(UOp.Not, EExists(descField(name))))
      .reduceLeft((left, right) => EBinary(BOp.And, left, right))

  private def descriptorCopy(name: String): Inst =
    IIf(
      EExists(descField(name)),
      IAssign(
        Field(
          Field(
            Field(Name("O"), EStr("__MAP__")),
            localRef("P"),
          ),
          EStr(name),
        ),
        ERef(descField(name)),
      ),
      ISeq(Nil),
    )

  private def descKeysExpr: Expr = EKeys(descRef, false)

  private def emptyDescriptorPrefix: List[Inst] =
    List(
      ILet(Name("descKeys"), descKeysExpr),
      IIf(
        EBinary(
          BOp.Eq,
          ESizeOf(localRef("descKeys")),
          EMath(0),
        ),
        IReturn(EBool(true)),
        ISeq(Nil),
      ),
    )

  private def descriptorCopyLoopPrefix: List[Inst] =
    List(
      ILet(Name("fields"), descKeysExpr),
      ILet(Name("idx"), EMath(0)),
      IWhile(
        EBinary(
          BOp.Lt,
          localRef("idx"),
          ESizeOf(localRef("fields")),
        ),
        ISeq(
          List(
            ILet(
              Name("f"),
              ERef(Field(Name("fields"), localRef("idx"))),
            ),
            IAssign(
              Field(
                Field(
                  Field(Name("O"), EStr("__MAP__")),
                  localRef("P"),
                ),
                localRef("f"),
              ),
              ERef(Field(Name("Desc"), localRef("f"))),
            ),
            IAssign(
              Name("idx"),
              EBinary(BOp.Add, localRef("idx"), EMath(1)),
            ),
          ),
        ),
      ),
    )

  private[fv] def countDescKeys(inst: Inst): Int = {
    var count = 0
    val scan = new esmeta.ir.util.UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case EKeys(ERef(Name("Desc")), false) =>
          count += 1
          super.walk(expr)
        case _ => super.walk(expr)
    }
    scan.walk(inst)
    count
  }

  /** BigInt.asIntN computes `mod < 2 ** (bits - 1)`. ESMeta can represent the
    * `bits == 0` intermediate as Math(0.5), while Rocq's deliberately
    * integer-only VMath cannot. After ToIndex, bits is nonnegative; when it is
    * zero, `mod` is `bigint modulo 1`, hence zero. Guarding the original
    * condition with `0 < bits` therefore skips only the unrepresentable
    * intermediate and selects the existing zero result.
    *
    * Match the complete ToIndex/ToBigInt completion-unwrapping flow through the
    * adjacent modulo/threshold pair, and fail closed on any specification or
    * compiler drift. Coercion and modulo evaluation remain in their original
    * order.
    */
  private[fv] def asIntNModuloExpr: Expr =
    EBinary(
      BOp.Mod,
      EConvert(COp.ToMath, localRef("bigint")),
      EBinary(BOp.Pow, EMath(2), localRef("bits")),
    )

  private[fv] def asIntNThresholdCondition: Expr =
    EUnary(
      UOp.Not,
      EBinary(
        BOp.Lt,
        localRef("mod"),
        EBinary(
          BOp.Pow,
          EMath(2),
          EBinary(BOp.Sub, localRef("bits"), EMath(1)),
        ),
      ),
    )

  private[fv] def asIntNGuardedCondition: Expr =
    EBinary(
      BOp.And,
      EBinary(BOp.Lt, EMath(0), localRef("bits")),
      asIntNThresholdCondition,
    )

  private[fv] def countExprOccurrences(inst: Inst, target: Expr): Int = {
    var count = 0
    val scan = new esmeta.ir.util.UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case _ if expr == target =>
          count += 1
          super.walk(expr)
        case _ => super.walk(expr)
    }
    scan.walk(inst)
    count
  }

  /** The compiled Number::remainder algorithm reaches this suffix only after
    * handling NaN, infinities, and zero operands. Its division/floor expansion
    * is not representable by the integer-only Rocq Math fragment. Preserve the
    * two Number-to-Math conversions, use the existing floored Math modulo, and
    * adjust a nonzero result by the divisor when the operand signs differ; this
    * recovers the specification's dividend-sign truncated remainder.
    */
  private[fv] def numberRemainderOriginalSuffix: List[Inst] = {
    val n = localRef("n")
    val d = localRef("d")
    val quotient = localRef("quotient")
    val temp = ERef(Temp(0))
    val r = localRef("r")
    List(
      IAssert(EYet("_n_ and _d_ are finite and non-zero")),
      ILet(
        Name("quotient"),
        EBinary(
          BOp.Div,
          EConvert(COp.ToMath, n),
          EConvert(COp.ToMath, d),
        ),
      ),
      IAssign(Temp(0), quotient),
      IIf(
        EBinary(BOp.Lt, temp, EMath(0)),
        IAssign(
          Temp(0),
          EUnary(UOp.Neg, EUnary(UOp.Floor, EUnary(UOp.Neg, temp))),
        ),
        IAssign(Temp(0), EUnary(UOp.Floor, temp)),
      ),
      ILet(Name("q"), temp),
      ILet(
        Name("r"),
        EBinary(
          BOp.Sub,
          EConvert(COp.ToMath, n),
          EBinary(BOp.Mul, EConvert(COp.ToMath, d), localRef("q")),
        ),
      ),
      IIf(
        EBinary(
          BOp.And,
          EBinary(BOp.Equal, r, EMath(0)),
          EBinary(BOp.Lt, n, ENumber(-0.0)),
        ),
        ISeq(List(IReturn(ENumber(-0.0)))),
        ISeq(Nil),
      ),
      IReturn(EConvert(COp.ToNumber, r)),
    )
  }

  private[fv] def numberRemainderNormalizedSuffix: List[Inst] = {
    val n = localRef("n")
    val d = localRef("d")
    val nMath = localRef("nM")
    val dMath = localRef("dM")
    val r = localRef("r")
    val nNegative = EBinary(BOp.Lt, n, ENumber(-0.0))
    val dNegative = EBinary(BOp.Lt, d, ENumber(-0.0))
    val oppositeSigns =
      EBinary(
        BOp.Or,
        EBinary(BOp.And, nNegative, EUnary(UOp.Not, dNegative)),
        EBinary(BOp.And, EUnary(UOp.Not, nNegative), dNegative),
      )
    List(
      IAssert(EYet("_n_ and _d_ are finite and non-zero")),
      ILet(Name("nM"), EConvert(COp.ToMath, n)),
      ILet(Name("dM"), EConvert(COp.ToMath, d)),
      ILet(Name("r"), EBinary(BOp.Mod, nMath, dMath)),
      IIf(
        EBinary(
          BOp.And,
          EUnary(UOp.Not, EBinary(BOp.Equal, r, EMath(0))),
          oppositeSigns,
        ),
        IAssign(Name("r"), EBinary(BOp.Sub, r, dMath)),
        ISeq(Nil),
      ),
      IIf(
        EBinary(
          BOp.And,
          EBinary(BOp.Equal, r, EMath(0)),
          EBinary(BOp.Lt, n, ENumber(-0.0)),
        ),
        ISeq(List(IReturn(ENumber(-0.0)))),
        ISeq(Nil),
      ),
      IReturn(EConvert(COp.ToNumber, r)),
    )
  }

  private[fv] def countInstSuffixOccurrences(
    inst: Inst,
    suffix: List[Inst],
  ): Int = inst match
    case ISeq(insts) =>
      insts.sliding(suffix.length).count(_.toList == suffix) +
      insts.map(countInstSuffixOccurrences(_, suffix)).sum
    case IIf(_, thenInst, elseInst, _) =>
      countInstSuffixOccurrences(thenInst, suffix) +
      countInstSuffixOccurrences(elseInst, suffix)
    case IWhile(_, body) => countInstSuffixOccurrences(body, suffix)
    case _               => 0

  private[fv] def truncOriginalAssign(tmp: Local): Inst =
    IAssign(tmp, EConvert(COp.ToMath, localRef("number")))

  private[fv] def truncReplacementAssign(tmp: Local): Inst =
    IAssign(
      tmp,
      EConvert(
        COp.ToMath,
        EConvert(COp.ToBigInt, localRef("number")),
      ),
    )

  private[fv] def truncTowardZero(tmp: Local): Inst = {
    val value = ERef(tmp)
    IIf(
      EBinary(BOp.Lt, value, EMath(0)),
      IAssign(
        tmp,
        EUnary(UOp.Neg, EUnary(UOp.Floor, EUnary(UOp.Neg, value))),
      ),
      IAssign(tmp, EUnary(UOp.Floor, value)),
    )
  }

  private def countAdjacentTruncPairs(
    inst: Inst,
    first: Local => Inst,
  ): Int = inst match
    case ISeq(insts) =>
      val here = insts.zipWithIndex.count { (assign, idx) =>
        assign match
          case IAssign(local: Local, _) =>
            idx + 1 < insts.length &&
            assign == first(local) &&
            insts(idx + 1) == truncTowardZero(local)
          case _ => false
      }
      here + insts.map(countAdjacentTruncPairs(_, first)).sum
    case IIf(_, thenInst, elseInst, _) =>
      countAdjacentTruncPairs(thenInst, first) +
      countAdjacentTruncPairs(elseInst, first)
    case IWhile(_, body) => countAdjacentTruncPairs(body, first)
    case _               => 0

  private def normalCompletionGuard(
    cond: Expr,
    tmp: Temp,
    result: Expr,
  ): Inst =
    IIf(
      cond,
      ISeq(
        List(
          ICall(tmp, EClo("NormalCompletion", Nil), List(result)),
          IReturn(ERef(tmp)),
        ),
      ),
      ISeq(Nil),
    )

  private def truncationGuards(name: String): List[Inst] = {
    val number = localRef("number")
    val nan = EBinary(BOp.Eq, number, ENumber(Double.NaN))
    val posZero = EBinary(BOp.Eq, number, ENumber(0.0))
    val negZero = EBinary(BOp.Eq, number, ENumber(-0.0))
    val posInf = EBinary(BOp.Eq, number, ENumber(Double.PositiveInfinity))
    val negInf = EBinary(BOp.Eq, number, ENumber(Double.NegativeInfinity))
    name match
      case "ToIntegerOrInfinity" =>
        List(
          normalCompletionGuard(
            EBinary(BOp.Or, EBinary(BOp.Or, nan, posZero), negZero),
            Temp(1),
            EMath(0),
          ),
          normalCompletionGuard(posInf, Temp(2), EInfinity(true)),
          normalCompletionGuard(negInf, Temp(3), EInfinity(false)),
        )
      case "ToInt32" | "ToUint32" | "ToUint16" =>
        List(
          normalCompletionGuard(
            EBinary(
              BOp.Or,
              EBinary(
                BOp.Or,
                nan,
                EBinary(BOp.Or, posInf, negInf),
              ),
              EBinary(BOp.Or, posZero, negZero),
            ),
            Temp(1),
            ENumber(0.0),
          ),
        )
      case _ => Nil
  }

  private def finiteNumberProducerPrefix: List[Inst] =
    List(
      ICall(
        Temp(0),
        EClo("ToNumber", Nil),
        List(localRef("argument")),
      ),
      completionAssert(Temp(0)),
      unwrapCompletion(Temp(0)),
      ILet(Name("number"), ERef(Temp(0))),
    )

  private[fv] def normalizeFiniteNumberTruncation(f: Func): Func = {
    val supportedNames =
      Set("ToIntegerOrInfinity", "ToInt32", "ToUint32", "ToUint16")
    val expectedReturn =
      if (f.name == "ToIntegerOrInfinity") "Normal[Int | INF] | Throw"
      else "Normal[NumberInt] | Throw"
    val metadataMatches =
      supportedNames.contains(f.name) &&
      !f.main &&
      f.kind == FuncKind.AbsOp &&
      (f.params match
        case List(param) =>
          param.lhs == Name("argument") &&
          !param.optional &&
          param.ty.ty == ESValueT
        case _ => false
      )
    val returnMatches = f.retTy.ty.toString == expectedReturn
    val insts = f.body match
      case ISeq(insts) => insts
      case _           => Nil

    def pairIndices(first: Local => Inst): List[(Int, Local)] =
      insts.zipWithIndex.collect {
        case (assign @ IAssign(tmp: Local, _), idx)
            if idx + 1 < insts.length &&
            assign == first(tmp) &&
            insts(idx + 1) == truncTowardZero(tmp) =>
          idx -> tmp
      }

    val topOriginalPairs = pairIndices(truncOriginalAssign)
    val topReplacementPairs = pairIndices(truncReplacementAssign)
    val originalPairs = countAdjacentTruncPairs(f.body, truncOriginalAssign)
    val replacementPairs =
      countAdjacentTruncPairs(f.body, truncReplacementAssign)
    val guardMatches = topOriginalPairs match
      case List((idx, _)) =>
        val expected = finiteNumberProducerPrefix ::: truncationGuards(f.name)
        idx == expected.length && insts.take(idx) == expected
      case _ => false

    if (
      !metadataMatches ||
      !returnMatches ||
      originalPairs != 1 ||
      replacementPairs != 0 ||
      topOriginalPairs.size != 1 ||
      topReplacementPairs.nonEmpty ||
      !guardMatches
    )
      throw Unsupported(
        s"${f.name} finite Number truncation shape drift: " +
        s"metadata=$metadataMatches, return=$returnMatches, " +
        s"original pairs=$originalPairs (top=${topOriginalPairs.size}), " +
        s"replacement pairs=$replacementPairs " +
        s"(top=${topReplacementPairs.size}), guards=$guardMatches",
      )

    val (pairIdx, tmp) = topOriginalPairs.head
    val normalized =
      f.copy(body = ISeq(insts.updated(pairIdx, truncReplacementAssign(tmp))))
    val remainingOriginal =
      countAdjacentTruncPairs(normalized.body, truncOriginalAssign)
    val normalizedReplacement =
      countAdjacentTruncPairs(normalized.body, truncReplacementAssign)
    if (remainingOriginal != 0 || normalizedReplacement != 1)
      throw Unsupported(
        s"${f.name} finite Number truncation normalization failed: " +
        s"original pairs=$originalPairs->$remainingOriginal, " +
        s"replacement pairs=$replacementPairs->$normalizedReplacement",
      )
    normalized
  }

  private[fv] def integralFloorDivOriginal(
    numerator: Expr,
    denominator: Expr,
  ): Expr =
    EUnary(UOp.Floor, EBinary(BOp.Div, numerator, denominator))

  private[fv] def integralFloorDivReplacement(
    numerator: Expr,
    denominator: Expr,
  ): Expr =
    EBinary(
      BOp.Div,
      EBinary(
        BOp.Sub,
        numerator,
        EBinary(BOp.Mod, numerator, denominator),
      ),
      denominator,
    )

  private def completionAssert(tmp: Temp): Inst =
    IAssert(ETypeCheck(ERef(tmp), Type(CompT)))

  private def unwrapCompletion(tmp: Temp): Inst =
    IIf(
      ETypeCheck(ERef(tmp), Type(AbruptT)),
      IReturn(ERef(tmp)),
      IAssign(tmp, ERef(Field(tmp, EStr("Value")))),
      true,
    )

  private def reverseLengthPrerequisites(name: String): List[Inst] =
    name match
      case "INTRINSICS.Array.prototype.reverse" =>
        List(
          ICall(
            Temp(1),
            EClo("LengthOfArrayLike", Nil),
            List(localRef("O")),
          ),
          completionAssert(Temp(1)),
          unwrapCompletion(Temp(1)),
          ILet(Name("len"), ERef(Temp(1))),
        )
      case "INTRINSICS.TypedArray.prototype.reverse" =>
        List(
          ICall(
            Temp(1),
            EClo("TypedArrayLength", Nil),
            List(localRef("taRecord")),
          ),
          ILet(Name("len"), ERef(Temp(1))),
        )
      case _ => Nil

  private def typedArrayLengthPrerequisites: List[Inst] = {
    def field(base: Local, name: String): Expr =
      ERef(Field(base, EStr(name)))
    val taRecord = Name("taRecord")
    val o = Name("O")
    val arrayLength = field(o, "ArrayLength")
    List(
      ICall(
        Temp(0),
        EClo("IsTypedArrayOutOfBounds", Nil),
        List(ERef(taRecord)),
      ),
      IAssert(EBinary(BOp.Eq, ERef(Temp(0)), EBool(false))),
      ILet(o, field(taRecord, "Object")),
      IIf(
        EUnary(
          UOp.Not,
          EBinary(BOp.Eq, arrayLength, EEnum("auto")),
        ),
        ISeq(List(IReturn(arrayLength))),
        ISeq(Nil),
      ),
      ICall(
        Temp(1),
        EClo("IsFixedLengthArrayBuffer", Nil),
        List(field(o, "ViewedArrayBuffer")),
      ),
      IAssert(EBinary(BOp.Eq, ERef(Temp(1)), EBool(false))),
      ILet(Name("byteOffset"), field(o, "ByteOffset")),
      ICall(
        Temp(2),
        EClo("TypedArrayElementSize", Nil),
        List(ERef(o)),
      ),
      ILet(Name("elementSize"), ERef(Temp(2))),
      ILet(
        Name("byteLength"),
        field(taRecord, "CachedBufferByteLength"),
      ),
      IAssert(
        EUnary(
          UOp.Not,
          EBinary(
            BOp.Eq,
            localRef("byteLength"),
            EEnum("detached"),
          ),
        ),
      ),
    )
  }

  private def countIntegralFloorDiv(
    inst: Inst,
    target: Expr,
  ): Int = countExprOccurrences(inst, target)

  private[fv] def normalizeIntegralFloorDivision(f: Func): Func = {
    val reverseNames = Set(
      "INTRINSICS.Array.prototype.reverse",
      "INTRINSICS.TypedArray.prototype.reverse",
    )
    val isReverse = reverseNames.contains(f.name)
    val isTypedArrayLength = f.name == "TypedArrayLength"
    val metadataMatches =
      if (isReverse)
        !f.main &&
        f.kind == FuncKind.Builtin &&
        f.params.map(param =>
          (param.lhs.name, param.optional, param.ty.ty.toString),
        ) == List(
          ("this", false, "ESValue"),
          ("ArgumentsList", false, "List[ESValue]"),
          ("NewTarget", false, "Record[Constructor] | Undefined"),
        ) &&
        f.retTy.ty.toString == "Unknown"
      else if (isTypedArrayLength)
        !f.main &&
        f.kind == FuncKind.AbsOp &&
        f.params.map(param =>
          (param.lhs.name, param.optional, param.ty.ty.toString),
        ) == List(
          (
            "taRecord",
            false,
            "Record[TypedArrayWithBufferWitnessRecord]",
          ),
        ) &&
        f.retTy.ty.toString == "Int[0+]"
      else false

    val numerator =
      if (isReverse) localRef("len")
      else
        EBinary(
          BOp.Sub,
          localRef("byteLength"),
          localRef("byteOffset"),
        )
    val denominator =
      if (isReverse) EMath(2)
      else localRef("elementSize")
    val original = integralFloorDivOriginal(numerator, denominator)
    val replacement = integralFloorDivReplacement(numerator, denominator)
    val originalCount = countIntegralFloorDiv(f.body, original)
    val replacementCount = countIntegralFloorDiv(f.body, replacement)
    val insts = f.body match
      case ISeq(insts) => insts
      case _           => Nil
    val topOriginalIndices = insts.zipWithIndex.collect {
      case (ILet(Name("middle"), expr), idx) if isReverse && expr == original =>
        idx
      case (IReturn(expr), idx) if isTypedArrayLength && expr == original => idx
    }
    val prerequisitesMatch = topOriginalIndices match
      case List(idx) if isReverse =>
        val expected = reverseLengthPrerequisites(f.name)
        idx >= expected.length &&
        insts.slice(idx - expected.length, idx) == expected
      case List(idx) if isTypedArrayLength =>
        idx == insts.length - 1 &&
        insts.take(idx) == typedArrayLengthPrerequisites
      case _ => false

    if (
      !metadataMatches ||
      originalCount != 1 ||
      replacementCount != 0 ||
      topOriginalIndices.size != 1 ||
      !prerequisitesMatch
    )
      throw Unsupported(
        s"${f.name} integral floor division shape drift: " +
        s"metadata=$metadataMatches, original=$originalCount, " +
        s"replacement=$replacementCount, " +
        s"top=${topOriginalIndices.size}, prerequisites=$prerequisitesMatch",
      )

    val idx = topOriginalIndices.head
    val rewritten = insts(idx) match
      case ILet(Name("middle"), expr) if isReverse && expr == original =>
        ILet(Name("middle"), replacement)
      case IReturn(expr) if isTypedArrayLength && expr == original =>
        IReturn(replacement)
      case _ =>
        throw Unsupported(s"${f.name} integral floor division rewrite drift")
    val normalized = f.copy(body = ISeq(insts.updated(idx, rewritten)))
    val remainingOriginal = countIntegralFloorDiv(normalized.body, original)
    val normalizedReplacement =
      countIntegralFloorDiv(normalized.body, replacement)
    if (remainingOriginal != 0 || normalizedReplacement != 1)
      throw Unsupported(
        s"${f.name} integral floor division normalization failed: " +
        s"original=$originalCount->$remainingOriginal, " +
        s"replacement=$replacementCount->$normalizedReplacement",
      )
    normalized
  }

  private def normalizeNumberRemainder(f: Func): Func = {
    val originalSuffix = numberRemainderOriginalSuffix
    val replacementSuffix = numberRemainderNormalizedSuffix
    val originalOccurrences =
      countInstSuffixOccurrences(f.body, originalSuffix)
    val replacementOccurrences =
      countInstSuffixOccurrences(f.body, replacementSuffix)
    val normalized = f.body match
      case ISeq(insts) if insts.endsWith(originalSuffix) =>
        f.copy(body =
          ISeq(insts.dropRight(originalSuffix.length) ::: replacementSuffix),
        )
      case _ => f
    val remainingOriginal =
      countInstSuffixOccurrences(normalized.body, originalSuffix)
    val normalizedReplacements =
      countInstSuffixOccurrences(normalized.body, replacementSuffix)
    if (
      originalOccurrences != 1 ||
      replacementOccurrences != 0 ||
      remainingOriginal != 0 ||
      normalizedReplacements != 1 ||
      normalized == f
    )
      throw Unsupported(
        "Number::remainder normalization shape drift: " +
        s"original suffixes=$originalOccurrences->$remainingOriginal, " +
        s"replacement suffixes=$replacementOccurrences->$normalizedReplacements",
      )
    normalized
  }

  private[fv] def numberMathTerminalExpr(
    op: FVInitState.NumberMathOp,
  ): Expr = {
    val (cop, bop, leftName, rightName) = op match
      case FVInitState.NumberMathOp.Add =>
        (COp.ToNumber, BOp.Add, "x", "y")
      case FVInitState.NumberMathOp.Mul =>
        (COp.ToNumber, BOp.Mul, "x", "y")
      case FVInitState.NumberMathOp.Div =>
        (COp.ToNumber, BOp.Div, "x", "y")
      case FVInitState.NumberMathOp.Pow =>
        (COp.ToApproxNumber, BOp.Pow, "base", "exponent")
    EConvert(
      cop,
      EBinary(
        bop,
        EConvert(COp.ToMath, localRef(leftName)),
        EConvert(COp.ToMath, localRef(rightName)),
      ),
    )
  }

  /** Validate the precise generated terminal shape consumed by the typed host
    * query. The IR is intentionally returned unchanged: replacing it with a raw
    * Number operator would change current ESMeta decimal-Math semantics.
    */
  private[fv] def validateNumberMathTerminal(
    f: Func,
    op: FVInitState.NumberMathOp,
  ): Func = {
    val (expectedName, expectedParams) = op match
      case FVInitState.NumberMathOp.Add => ("Number::add", List("x", "y"))
      case FVInitState.NumberMathOp.Mul =>
        ("Number::multiply", List("x", "y"))
      case FVInitState.NumberMathOp.Div =>
        ("Number::divide", List("x", "y"))
      case FVInitState.NumberMathOp.Pow =>
        ("Number::exponentiate", List("base", "exponent"))
    val expected = numberMathTerminalExpr(op)
    val occurrences = countExprOccurrences(f.body, expected)
    val isTerminal = f.body match
      case ISeq(insts) => insts.lastOption.contains(IReturn(expected))
      case _           => false
    val actualParams = f.params.map(_.lhs.name)
    val paramsAreRequiredNumbers =
      f.params.size == 2 && f.params.forall(param =>
        !param.optional && param.ty.ty == NumberT,
      )
    val metadataMatches =
      !f.main &&
      f.kind == FuncKind.NumMeth &&
      f.name == expectedName &&
      actualParams == expectedParams &&
      paramsAreRequiredNumbers &&
      f.retTy.ty == NumberT
    if (occurrences != 1 || !isTerminal || !metadataMatches)
      throw Unsupported(
        s"${f.name} Number Math terminal shape drift: " +
        s"op=$op, main=${f.main}, kind=${f.kind}, " +
        s"expected name=$expectedName, params=$actualParams, " +
        s"required Number params=$paramsAreRequiredNumbers, " +
        s"Number return=${f.retTy.ty == NumberT}, " +
        s"occurrences=$occurrences, terminal=$isTerminal",
      )
    f
  }

  private def normalizeBigIntAsIntN(f: Func): Func = {
    def completionAssert(local: Local): Inst =
      IAssert(ETypeCheck(ERef(local), Type(CompT)))

    def unwrapCompletion(local: Local): Inst =
      IIf(
        ETypeCheck(ERef(local), Type(AbruptT)),
        IReturn(ERef(local)),
        IAssign(local, ERef(Field(local, EStr("Value")))),
        true,
      )

    val originalModuloExpressions =
      countExprOccurrences(f.body, asIntNModuloExpr)
    val originalThresholds =
      countExprOccurrences(f.body, asIntNThresholdCondition)
    var rewrites = 0

    def rewrite(inst: Inst): Inst = inst match
      case ISeq(insts) => ISeq(rewriteSeq(insts))
      case IIf(cond, thenInst, elseInst, isAbruptInst) =>
        IIf(
          cond,
          rewrite(thenInst),
          rewrite(elseInst),
          isAbruptInst,
        )
      case IWhile(cond, body) => IWhile(cond, rewrite(body))
      case other              => other

    def rewriteSeq(insts: List[Inst]): List[Inst] = insts match
      case (bitsCall @ ICall(
            bitsTemp,
            EClo("ToIndex", Nil),
            List(bitsArg),
          )) ::
          bitsAssert ::
          bitsUnwrap ::
          bitsAssign ::
          (bigintCall @ ICall(
            bigintTemp,
            EClo("ToBigInt", Nil),
            List(bigintArg),
          )) ::
          bigintAssert ::
          bigintUnwrap ::
          bigintAssign ::
          (mod @ ILet(Name("mod"), expr)) ::
          IIf(cond, thenInst, elseInst, isAbruptInst) :: tail
          if bitsArg == localRef("bits") &&
          bitsAssert == completionAssert(bitsTemp) &&
          bitsUnwrap == unwrapCompletion(bitsTemp) &&
          bitsAssign == IAssign(Name("bits"), ERef(bitsTemp)) &&
          bigintArg == localRef("bigint") &&
          bigintAssert == completionAssert(bigintTemp) &&
          bigintUnwrap == unwrapCompletion(bigintTemp) &&
          bigintAssign == IAssign(Name("bigint"), ERef(bigintTemp)) &&
          expr == asIntNModuloExpr &&
          cond == asIntNThresholdCondition =>
        rewrites += 1
        List[Inst](
          bitsCall,
          bitsAssert,
          bitsUnwrap,
          bitsAssign,
          bigintCall,
          bigintAssert,
          bigintUnwrap,
          bigintAssign,
          mod,
          IIf(
            asIntNGuardedCondition,
            rewrite(thenInst),
            rewrite(elseInst),
            isAbruptInst,
          ),
        ) ::: rewriteSeq(tail)
      case head :: tail => rewrite(head) :: rewriteSeq(tail)
      case Nil          => Nil

    val normalized = f.copy(body = rewrite(f.body))
    val normalizedModuloExpressions =
      countExprOccurrences(normalized.body, asIntNModuloExpr)
    val normalizedThresholds =
      countExprOccurrences(normalized.body, asIntNThresholdCondition)
    val guardedThresholds =
      countExprOccurrences(normalized.body, asIntNGuardedCondition)
    if (
      rewrites != 1 ||
      originalModuloExpressions != 1 ||
      normalizedModuloExpressions != 1 ||
      originalThresholds != 1 ||
      normalizedThresholds != 1 ||
      guardedThresholds != 1
    )
      throw Unsupported(
        "INTRINSICS.BigInt.asIntN normalization shape drift: " +
        s"rewrites=$rewrites, modulo expressions=" +
        s"$originalModuloExpressions->$normalizedModuloExpressions, " +
        s"threshold expressions=$originalThresholds->$normalizedThresholds, " +
        s"guarded thresholds=$guardedThresholds",
      )
    normalized
  }

  private def normalizePropertyDescriptor(f: Func): Func = {
    val originalKeys = countDescKeys(f.body)
    var emptyRewrites = 0
    var copyRewrites = 0

    def rewrite(inst: Inst): Inst = inst match
      case ISeq(insts) => ISeq(rewriteSeq(insts))
      case IIf(cond, thenInst, elseInst, isAbruptInst) =>
        IIf(
          cond,
          rewrite(thenInst),
          rewrite(elseInst),
          isAbruptInst,
        )
      case IWhile(cond, body) => IWhile(cond, rewrite(body))
      case other              => other

    def rewriteSeq(insts: List[Inst]): List[Inst] =
      if (insts.startsWith(emptyDescriptorPrefix)) {
        emptyRewrites += 1
        IIf(
          descriptorEmptyTest,
          IReturn(EBool(true)),
          ISeq(Nil),
        ) :: rewriteSeq(insts.drop(emptyDescriptorPrefix.length))
      } else if (insts.startsWith(descriptorCopyLoopPrefix)) {
        copyRewrites += 1
        propertyDescriptorFields.map(descriptorCopy) :::
        rewriteSeq(insts.drop(descriptorCopyLoopPrefix.length))
      } else
        insts match
          case head :: tail => rewrite(head) :: rewriteSeq(tail)
          case Nil          => Nil

    val normalized = f.copy(body = rewrite(f.body))
    val remainingKeys = countDescKeys(normalized.body)
    if (
      originalKeys != 5 ||
      emptyRewrites != 1 ||
      copyRewrites != 4 ||
      remainingKeys != 0
    )
      throw Unsupported(
        "ValidateAndApplyPropertyDescriptor normalization shape drift: " +
        s"Desc EKeys=$originalKeys, empty rewrites=$emptyRewrites, " +
        s"copy rewrites=$copyRewrites, remaining=$remainingKeys",
      )
    normalized
  }

  /** TimeClip's limit is exactly 8.64 * 10^15. The extracted IR spells that as
    * a non-integral Math literal multiplied by an integral power, while the
    * Rocq Math fragment stores only integers. The exact integral result,
    * 8,640,000,000,000,000, is below 2^53 and therefore exactly representable
    * as a Binary64 Number. Compare the Number input directly against the two
    * Number boundaries. Besides preserving the finite-number predicate, this
    * deliberately avoids introducing typed Number/Math host queries that the
    * unnormalized ESMeta oracle did not capture.
    */
  private[fv] val timeClipOriginalLimit: Expr =
    EBinary(
      BOp.Mul,
      EMath(BigDecimal("8.64")),
      EBinary(BOp.Pow, EMath(10), EMath(15)),
    )

  private[fv] val timeClipIntegralLimit: Expr =
    EMath(BigDecimal("8640000000000000"))

  private[fv] def timeClipLimitCondition(limit: Expr): Expr =
    EBinary(
      BOp.Lt,
      limit,
      EUnary(UOp.Abs, EConvert(COp.ToMath, localRef("time"))),
    )

  private[fv] def timeClipOutOfRangeCondition: Expr = {
    val time = localRef("time")
    EBinary(
      BOp.Or,
      EBinary(
        BOp.Lt,
        time,
        ENumber(-8640000000000000.0),
      ),
      EBinary(
        BOp.Lt,
        ENumber(8640000000000000.0),
        time,
      ),
    )
  }

  private[fv] def normalizeTimeClip(f: Func): Func = {
    val metadataMatches =
      !f.main &&
      f.kind == FuncKind.AbsOp &&
      f.params.map(param =>
        (param.lhs.name, param.optional, param.ty.ty.toString),
      ) == List(("time", false, "Number")) &&
      f.retTy.ty.toString == "Number"
    val original = timeClipLimitCondition(timeClipOriginalLimit)
    val replacement = timeClipOutOfRangeCondition
    val insts = f.body match
      case ISeq(insts) => insts
      case _           => Nil
    val originalIndices = insts.zipWithIndex.collect {
      case (IIf(cond, ISeq(List(IReturn(ENumber(n)))), ISeq(Nil), false), idx)
          if cond == original && n.isNaN =>
        idx
    }
    val originalCount = countExprOccurrences(f.body, original)
    val replacementCount = countExprOccurrences(f.body, replacement)

    if (
      !metadataMatches ||
      originalIndices != List(1) ||
      originalCount != 1 ||
      replacementCount != 0
    )
      throw Unsupported(
        "TimeClip integral boundary normalization shape drift: " +
        s"metadata=$metadataMatches, indices=$originalIndices, " +
        s"conditions=$originalCount->$replacementCount",
      )

    val idx = originalIndices.head
    val branch = insts(idx).asInstanceOf[IIf]
    val normalized =
      f.copy(body = ISeq(insts.updated(idx, branch.copy(cond = replacement))))
    if (
      countExprOccurrences(normalized.body, original) != 0 ||
      countExprOccurrences(normalized.body, replacement) != 1
    )
      throw Unsupported("TimeClip integral boundary normalization failed")
    normalized
  }

  private val toUint8ClampMvYet =
    EYet("Let _mv_ be the extended mathematical value of _number_.")
  private val toUint8ClampTieYet =
    EYet(
      "If _f_ is even, return 𝔽(_f_). Otherwise, return 𝔽(_f_ + 1).",
    )

  private def originalClampReturn(
    result: Expr,
    valueTemp: Temp,
    retTemp: Temp,
  ): Inst =
    ISeq(
      List(
        IAssign(valueTemp, EConvert(COp.ToNumber, result)),
        IIf(
          ETypeCheck(ERef(valueTemp), Type(CompT)),
          IReturn(ERef(valueTemp)),
          ISeq(Nil),
        ),
        ICall(
          retTemp,
          EClo("NormalCompletion", Nil),
          List(ERef(valueTemp)),
        ),
        IReturn(ERef(retTemp)),
      ),
    )

  private[fv] def toUint8ClampOriginalSuffix: List[Inst] = {
    val clamped = localRef("clamped")
    val f = localRef("f")
    val midpoint = EBinary(BOp.Add, f, EMath(BigDecimal("0.5")))
    val next = EBinary(BOp.Add, f, EMath(1))
    List(
      IExpr(toUint8ClampMvYet),
      ICall(
        Temp(2),
        EClo("__CLAMP__", Nil),
        List(localRef("mv"), EMath(0), EMath(255)),
      ),
      ILet(Name("clamped"), ERef(Temp(2))),
      ILet(Name("f"), EUnary(UOp.Floor, clamped)),
      IIf(
        EBinary(BOp.Lt, clamped, midpoint),
        originalClampReturn(f, Temp(3), Temp(4)),
        ISeq(Nil),
      ),
      IIf(
        EBinary(BOp.Lt, midpoint, clamped),
        originalClampReturn(next, Temp(5), Temp(6)),
        ISeq(Nil),
      ),
      IExpr(toUint8ClampTieYet),
    )
  }

  private[fv] def toUint8ClampNormalizedSuffix: List[Inst] = {
    val number = localRef("number")
    val f = localRef("f")
    val fNumber = localRef("fNumber")
    val midpoint = localRef("midpoint")
    val rounded = Name("rounded")
    val next = EBinary(BOp.Add, fNumber, ENumber(1.0))
    List(
      normalCompletionGuard(
        EUnary(UOp.Not, EBinary(BOp.Lt, ENumber(0.0), number)),
        Temp(2),
        ENumber(0.0),
      ),
      normalCompletionGuard(
        EUnary(UOp.Not, EBinary(BOp.Lt, number, ENumber(255.0))),
        Temp(3),
        ENumber(255.0),
      ),
      ICall(
        Temp(4),
        EClo("ToIntegerOrInfinity", Nil),
        List(number),
      ),
      IAssert(ETypeCheck(ERef(Temp(4)), Type(NormalT))),
      IAssign(Temp(4), ERef(Field(Temp(4), EStr("Value")))),
      ILet(Name("f"), ERef(Temp(4))),
      ILet(Name("fNumber"), EConvert(COp.ToNumber, f)),
      ILet(
        Name("midpoint"),
        EBinary(BOp.Add, fNumber, ENumber(0.5)),
      ),
      ILet(rounded, fNumber),
      IIf(
        EBinary(BOp.Lt, number, midpoint),
        ISeq(Nil),
        IIf(
          EBinary(BOp.Lt, midpoint, number),
          IAssign(rounded, next),
          IIf(
            EBinary(
              BOp.Equal,
              EBinary(BOp.Mod, f, EMath(2)),
              EMath(0),
            ),
            ISeq(Nil),
            IAssign(rounded, next),
          ),
        ),
      ),
      ICall(
        Temp(5),
        EClo("NormalCompletion", Nil),
        List(ERef(rounded)),
      ),
      IReturn(ERef(Temp(5))),
    )
  }

  /** Re-express ToUint8Clamp in the existing executable fragment. The original
    * extraction contains an uncompiled mathematical-value step and an
    * uncompiled half-even tie. Clamp with Number comparisons, obtain the
    * positive finite floor through ToIntegerOrInfinity, and perform the final
    * midpoint/tie-even choice with Number arithmetic plus integral Math modulo.
    */
  private[fv] def normalizeToUint8Clamp(f: Func): Func = {
    val metadataMatches =
      !f.main &&
      f.kind == FuncKind.AbsOp &&
      f.params.map(param =>
        (param.lhs.name, param.optional, param.ty.ty.toString),
      ) == List(("argument", false, "ESValue")) &&
      f.retTy.ty.toString == "Normal[NumberInt] | Throw"
    val insts = f.body match
      case ISeq(insts) => insts
      case _           => Nil
    val original = toUint8ClampOriginalSuffix
    val replacement = toUint8ClampNormalizedSuffix
    val originalCount = countInstSuffixOccurrences(f.body, original)
    val replacementCount = countInstSuffixOccurrences(f.body, replacement)

    if (
      !metadataMatches ||
      !insts.endsWith(original) ||
      originalCount != 1 ||
      replacementCount != 0
    )
      throw Unsupported(
        "ToUint8Clamp half-even normalization shape drift: " +
        s"metadata=$metadataMatches, top suffix=${insts.endsWith(original)}, " +
        s"suffixes=$originalCount->$replacementCount",
      )

    val normalized = f.copy(
      body = ISeq(insts.dropRight(original.length) ::: replacement),
    )
    if (
      countInstSuffixOccurrences(normalized.body, original) != 0 ||
      countInstSuffixOccurrences(normalized.body, replacement) != 1 ||
      countExprOccurrences(normalized.body, toUint8ClampMvYet) != 0 ||
      countExprOccurrences(normalized.body, toUint8ClampTieYet) != 0
    )
      throw Unsupported("ToUint8Clamp half-even normalization failed")
    normalized
  }

  private[fv] def normalizeForRocq(f: Func): Func = f.name match
    case "INTRINSICS.BigInt.asIntN" => normalizeBigIntAsIntN(f)
    case "Number::remainder"        => normalizeNumberRemainder(f)
    case "ToIntegerOrInfinity" | "ToInt32" | "ToUint32" | "ToUint16" =>
      normalizeFiniteNumberTruncation(f)
    case "INTRINSICS.Array.prototype.reverse" |
        "INTRINSICS.TypedArray.prototype.reverse" | "TypedArrayLength" =>
      normalizeIntegralFloorDivision(f)
    case "Number::add" =>
      validateNumberMathTerminal(f, FVInitState.NumberMathOp.Add)
    case "Number::multiply" =>
      validateNumberMathTerminal(f, FVInitState.NumberMathOp.Mul)
    case "Number::divide" =>
      validateNumberMathTerminal(f, FVInitState.NumberMathOp.Div)
    case "Number::exponentiate" =>
      validateNumberMathTerminal(f, FVInitState.NumberMathOp.Pow)
    case "ValidateAndApplyPropertyDescriptor" =>
      normalizePropertyDescriptor(f)
    case "TimeClip"     => normalizeTimeClip(f)
    case "ToUint8Clamp" => normalizeToUint8Clamp(f)
    case _              => f

  def rocqFunc(f: Func)(using CFG): String = {
    val normalized = normalizeForRocq(f)
    val params = normalized.params.map(p => strLit(p.lhs.name))
    val paramTypes = normalized.params.map(rocqParamAnnotation)
    s"mkTypedFunc ${normalized.main} ${rocqFuncKind(normalized.kind)} " +
    s"${strLit(normalized.name)} ${coqList(params)} " +
    s"${coqList(paramTypes)} ${rocqTypeAnnotation(normalized.retTy)} " +
    s"${rocqInst(normalized.body)}"
  }

  /** observable values (address-free per the observable-behavior spec) */
  def rocqValue(v: Value): String = v match
    case Math(d) =>
      if (!d.isWhole) throw Unsupported(s"non-integer Math value: $d")
      s"(VMath ${zLit(d.toBigInt)})"
    case Bool(b)     => s"(VBool $b)"
    case Str(s)      => s"(VStr ${cstrLit(s)})"
    case Undef       => "VUndef"
    case Null        => "VNull"
    case Enum(name)  => s"(VEnum ${strLit(name)})"
    case Number(d)   => s"(VNumber ${floatLit(d)})"
    case CodeUnit(c) => s"(VCodeUnit ${c.toInt})"
    case Infinity(p) => s"(VInfinity $p)"
    case BigInt(n)   => s"(VBigInt ${zLit(n)})"
    case GrammarSymbol(name, params) =>
      s"(VGrammarSymbol ${strLit(name)} ${coqList(params.map(_.toString))})"
    case _ =>
      throw Unsupported(s"observable value: ${v.getClass.getSimpleName}")

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
      val cfg = CFGBuilder(program)
      given CFG = cfg

      // Exec.v deliberately has no nonlocal-control machine.  ECont is
      // exported for the specification/ITree path, but standalone
      // vm_compute expectations must not pretend that Exec validates it.
      var hasCont = false
      val contScan = new esmeta.ir.util.UnitWalker {
        override def walk(expr: Expr): Unit = expr match
          case ECont(_) => hasCont = true
          case _        => super.walk(expr)
      }
      contScan.walk(program)
      if (hasCont)
        throw Unsupported("ECont requires the ITree continuation executor")

      val id = sanitize(
        path.stripSuffix(".ir").split("/").takeRight(2).mkString("_"),
      )

      // translate first: rejects out-of-fragment constructs
      val funcDefs = funcs.zipWithIndex.map {
        case (f, i) => (s"${id}_f$i", rocqFunc(f))
      }

      // run with ESMeta's interpreter, capturing observables
      val st = State(cfg)
      val interp = new CapturingInterpreter(st)
      val finalSt = interp.result
      if (interp.failedToEvaluateAsserts > 0)
        throw Unsupported(
          s"ESMeta silently skipped ${interp.failedToEvaluateAsserts} " +
          "non-EYet assertion(s) that failed to evaluate; " +
          s"${interp.skippedYetAsserts} bare EYet assertion(s) were " +
          "intentional no-ops",
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
      case Failure(err)              => throw err

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
         |From Stdlib Require Import String ZArith List Floats PString.
         |Import ListNotations.
         |From ESMetaFV Require Import Fragment Domain Exec TestEncoding.
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
