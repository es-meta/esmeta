package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.state.*
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

  /** interpreter that captures prints instead of writing to stdout */
  class CapturingInterpreter(st0: State)
    extends Interpreter(st0, timeLimit = Some(10)) {
    val prints: ListBuffer[Value] = ListBuffer()
    override def eval(inst: NormalInst): Unit = inst match
      case IPrint(expr) => prints += eval(expr)
      case _            => super.eval(inst)
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
    case BOp.Equal => "BEqual"
    case _         => throw Unsupported(s"bop: $op")

  /** map an ESMeta type to the model's restricted [tyexp] (ADR-11).
    * Uses ESMeta's own stringification with a strict whitelist. */
  def rocqTy(t: Type): String = t.ty.toString match
    case "Record[CompletionRecord]" => "TCompletion"
    case "Record[AbruptCompletion]" => "TAbrupt"
    case "Record[NormalCompletion]" => "TNormal"
    case "List"                     => "TList"
    case "Map"                      => "TMapTy"
    case "String"                   => "TStrTy"
    case "Boolean"                  => "TBoolTy"
    case "Math"                     => "TMathTy"
    case "Undefined"                => "TUndefTy"
    case "Null"                     => "TNullTy"
    case s if s.startsWith("Record[") && s.endsWith("]") && !s.contains(",") =>
      val name = s.stripPrefix("Record[").stripSuffix("]")
      if (name.forall(c => c.isLetterOrDigit || c == '.' || c == '_'))
        s"(TRecord ${strLit(name)})"
      else throw Unsupported(s"ty: $s")
    case s => throw Unsupported(s"ty: $s")

  def rocqExpr(e: Expr): String = e match
    case EMath(n) =>
      if (!n.isWhole) throw Unsupported(s"non-integer Math literal: $n")
      s"(EMath ${zLit(n.toBigInt)})"
    case EBool(b)         => s"(EBool $b)"
    case EStr(s)          => s"(EStr ${strLit(s)})"
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
    val params = f.params.map { p =>
      if (p.optional) throw Unsupported(s"optional param: ${p.lhs.name}")
      strLit(p.lhs.name)
    }
    s"mkFunc ${f.main} ${strLit(f.name)} ${coqList(params)} ${rocqInst(f.body)}"
  }

  /** observable values (address-free per the observable-behavior spec) */
  def rocqValue(v: Value): String = v match
    case Math(d) =>
      if (!d.isWhole) throw Unsupported(s"non-integer Math value: $d")
      s"(VMath ${zLit(d.toBigInt)})"
    case Bool(b)    => s"(VBool $b)"
    case Str(s)     => s"(VStr ${strLit(s)})"
    case Undef      => "VUndef"
    case Null       => "VNull"
    case Enum(name) => s"(VEnum ${strLit(name)})"
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
        walkTree(s"$BASE_DIR/tests/ir")
          .filter(f => irFilter(f.getName))
          .map(_.toString)
          .toList
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
         |From Stdlib Require Import String ZArith List.
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
