package esmeta.fv

import esmeta.cfg.CFG
import esmeta.ir.*
import esmeta.spec.Terminal

/** Structural ESMeta-IR to Gallina/CRIS ITree compiler.
  *
  * This is deliberately separate from [[FVExport.rocqExpr]]: emitted terms
  * contain no IR values and never invoke the generic denoter. Primitive
  * operations are delegated to evaluated-value combinators in
  * `DirectSemantics.v`; evaluation order and control structure stay explicit
  * here.
  */
object FVDirectExport {
  import FVExport.{Unsupported, coqList, cstrLit, floatLit, local, natLit, strLit, zLit}

  final case class DirectFuncDefs(
    funId: String,
    instName: String,
    ordinaryName: String,
    continuationName: String,
    source: String,
    main: Boolean,
  ) {
    def ordinaryEntry(fnames: String): String =
      s"direct_fnsem mn ${strLit(funId)} ($ordinaryName mn $fnames)"

    def continuationEntry(fnames: String): String =
      s"direct_cont_fnsem mn ${strLit(funId)} ($continuationName mn $fnames)"

    def mainEntry(fnames: String): Option[String] =
      Option.when(main)(
        s"snd (direct_entry mn ($ordinaryName mn $fnames))",
      )
  }

  /** Stable ordered clause identifiers. The list mirrors the source order in
    * `Semantics.denote_ref`, `denote_expr`, and `denote_inst`, including
    * shape-specialized clauses whose precedence is semantically relevant.
    */
  val orderedClauseIds: List[String] = List(
    "ref.RVar",
    "ref.RField",
    "expr.EMath",
    "expr.EBool",
    "expr.EStr",
    "expr.EUndef",
    "expr.ENull",
    "expr.EEnum",
    "expr.ERef",
    "expr.EUnary",
    "expr.EBinary.BAnd",
    "expr.EBinary.BOr",
    "expr.EBinary.NumberMathComparison",
    "expr.EBinary.general",
    "expr.EClo",
    "expr.ECont",
    "expr.EList",
    "expr.ESizeOf",
    "expr.ERecord",
    "expr.EExists",
    "expr.ETypeOf",
    "expr.ETypeCheck",
    "expr.EYet",
    "expr.EMap",
    "expr.EKeys",
    "expr.ECopy",
    "expr.ENumber",
    "expr.EBigInt",
    "expr.EInfinity",
    "expr.ECodeUnit",
    "expr.EConvert.ToNumber.AddMath",
    "expr.EConvert.ToNumber.MulMath",
    "expr.EConvert.ToNumber.DivMath",
    "expr.EConvert.ToApproxNumber.PowMath",
    "expr.EConvert.ToApproxNumber.SinMath",
    "expr.EConvert.general",
    "expr.EToStr",
    "expr.EVariadic",
    "expr.EMathOp",
    "expr.EContains",
    "expr.ETrim",
    "expr.ESyntactic",
    "expr.EGrammarSymbol",
    "expr.EInstanceOf",
    "expr.ESourceText",
    "expr.EParse",
    "expr.ESubstring",
    "inst.INop",
    "inst.ISeq",
    "inst.IExpr",
    "inst.ILet",
    "inst.IAssign",
    "inst.IIf",
    "inst.IWhile",
    "inst.ICall",
    "inst.IReturn",
    "inst.IAssert.EYet",
    "inst.IAssert.general",
    "inst.IPrint",
    "inst.IPush",
    "inst.IPop",
    "inst.IExpand",
    "inst.IDelete",
    "inst.ISdoCall",
  )

  /** Compile an already-normalized function. Callers producing generic and
    * direct artifacts must normalize once and pass this same object to
    * [[FVExport.rocqNormalizedFunc]] and this method.
    */
  def compileNormalized(id: String, f: Func)(using CFG): DirectFuncDefs = {
    if (!id.matches("[A-Za-z_][A-Za-z0-9_']*"))
      throw Unsupported(s"direct function ${f.name}: invalid Gallina id $id")
    val compiler = Compiler(f.name)
    val instName = s"${id}_inst"
    val ordinaryName = id
    val continuationName = s"${id}_cont"
    val params = coqList(f.params.map(p => strLit(p.lhs.name)))
    val body =
      try compiler.inst(f.body, "fnames", "ρ")
      catch
        case error: Unsupported
            if !error.getMessage.startsWith(s"direct function ${f.name},") =>
          throw Unsupported(
            s"direct function ${f.name}, clause export: ${error.getMessage}",
          )
    // The margin marker is `#`, not `|`: the formatted body puts match
    // branches at the start of a line, and stripMargin would eat their `|`.
    val source =
      s"""Section ${id}_section.
         #Context `{!crisG Γ Σ α β τ _S _I}.
         #
         #Definition $instName (mn : string) : direct_inst_body :=
         #  fun fnames ρ =>
         #    ${formatTerm(body, 4)}.
         #
         #Definition $ordinaryName (mn : string) (fnames : list string) : ir_arg -> itree crisE val :=
         #  direct_fbody fnames $params ${f.main} ($instName mn).
         #
         #Definition $continuationName (mn : string) (fnames : list string) : ir_arg -> itree crisE val :=
         #  direct_cont_fbody fnames $params ${f.main} ($instName mn).
         #End ${id}_section.
         #""".stripMargin('#')
    val forbidden = List("denote_expr", "denote_ref", "denote_inst", "denote_fbody")
    forbidden.find(source.contains).foreach { name =>
      throw Unsupported(s"direct function ${f.name}: forbidden fallback $name")
    }
    DirectFuncDefs(
      f.name,
      instName,
      ordinaryName,
      continuationName,
      source,
      f.main,
    )
  }

  /** Convenience entry point for standalone callers. Whole-backend generation
    * should normalize once explicitly and use [[compileNormalized]].
    */
  def compile(id: String, f: Func)(using CFG): DirectFuncDefs =
    compileNormalized(id, FVExport.normalizeForRocq(f))

  /** Scope-safe negative literals.
    *
    * A direct shard has to import CRIS to elaborate its `crisG` context, and
    * CRIS brings notations that claim the leading `-`: Rocq then reads
    * `(-1.0)%float` as a unary minus applied to `1` and stops at the `.`.
    * Generic shards import neither CRIS nor Semantics, so
    * [[FVExport.zLit]] and [[FVExport.floatLit]] stay as they are and their
    * output does not move.
    */
  private val negFloatLit = """\(-(.*)\)%float""".r

  private def directFloatLit(d: Double): String = floatLit(d) match
    case negFloatLit(magnitude) => s"(- ($magnitude)%float)%float"
    case other                  => other

  private def directZLit(n: scala.math.BigInt): String =
    if (n < 0) s"(- (${-n}))%Z" else zLit(n)

  /** Lay a generated term out over several lines.
    *
    * The compiler builds one flat string, which for a real spec function is a
    * single line thousands of characters wide. Rocq does not care, but a
    * reader opening one of these files does. Whitespace is the only thing
    * this changes: a break after each `;;`/`;;;` and before each match
    * branch, indented by parenthesis depth.
    *
    * Depth is capped because the terms nest far deeper than any indentation
    * can usefully track; past the cap, the `;;` breaks alone carry the shape.
    */
  private def formatTerm(term: String, baseIndent: Int): String = {
    val maxDepth = 12
    val out = new StringBuilder
    var depth = 0
    var index = 0
    def trimTrailingSpaces(): Unit =
      while (out.nonEmpty && out.last == ' ') out.deleteCharAt(out.length - 1)
    def onFreshLine: Boolean = {
      var back = out.length - 1
      while (back >= 0 && out(back) == ' ') back -= 1
      back < 0 || out(back) == '\n'
    }
    def newline(level: Int = depth): Unit = {
      trimTrailingSpaces()
      out += '\n'
      out ++= " " * (baseIndent + 2 * math.min(level, maxDepth))
    }
    def skipSpace(): Unit =
      if (index < term.length && term(index) == ' ') index += 1
    while (index < term.length) {
      if (term(index) == '"') {
        // Copy the literal verbatim; Gallina escapes a quote as "".
        out += '"'
        index += 1
        var closed = false
        while (index < term.length && !closed) {
          if (term(index) != '"') { out += term(index); index += 1 }
          else if (index + 1 < term.length && term(index + 1) == '"') {
            out ++= "\"\""; index += 2
          } else { out += '"'; index += 1; closed = true }
        }
      } else if (term.startsWith(";;;", index)) {
        out ++= ";;;"; index += 3; skipSpace(); newline()
      } else if (term.startsWith(";;", index)) {
        out ++= ";;"; index += 2; skipSpace(); newline()
      } else if (term.startsWith("<- (", index)) {
        // A bind whose operand is itself a chain: start it one level in.
        out ++= "<-"; index += 2; skipSpace(); newline(depth + 1)
      } else if (term.startsWith("with ", index)) {
        out ++= "with"; index += 5; newline()
      } else if (term.startsWith("| ", index)) {
        if (!onFreshLine) newline()
        out ++= "| "; index += 2
      } else if (term.startsWith("end", index) && !onFreshLine) {
        newline(); out ++= "end"; index += 3
      } else {
        term(index) match
          case '(' => depth += 1
          case ')' => depth = math.max(0, depth - 1)
          case _   => ()
        out += term(index)
        index += 1
      }
    }
    out.toString
  }

  private final class Compiler(functionName: String)(using cfg: CFG) {
    private var serial = 0
    private def fresh(prefix: String): String = {
      serial += 1
      s"${prefix}_$serial"
    }
    private def fail(clause: String, detail: String): Nothing =
      throw Unsupported(s"direct function $functionName, clause $clause: $detail")
    private def bind(name: String, term: String, next: String): String =
      s"($name <- $term;; $next)"
    private def seq(first: String, next: String): String = s"($first;;; $next)"

    private def exprList(es: List[Expr], rho: String): String = es match
      case Nil => "Ret nil"
      case head :: tail =>
        val v = fresh("v")
        val vs = fresh("vs")
        bind(v, expr(head, rho), bind(vs, exprList(tail, rho), s"Ret ($v :: $vs)"))

    private def parseRef(r: Ref, rho: String): String = r match
      case v: Var => s"eval_ret (TVar ${rocqVar(v)})"
      case Field(base, field) =>
        val target = fresh("target")
        val baseValue = fresh("base")
        val fieldValue = fresh("field")
        s"eval_bind (${parseRef(base, rho)}) (fun $target => " +
          s"eval_bind (eval_read_target mn $rho $target) (fun $baseValue => " +
          s"eval_bind (${parseOperand(field, rho)}) (fun $fieldValue => " +
          s"eval_ret (TField $baseValue $fieldValue))))"

    private def parseOperand(e: Expr, rho: String): String = e match
      case EMath(n) =>
        if (!n.isWhole) fail("expr.EParse", s"non-integer Math literal $n")
        s"eval_ret (VMath ${directZLit(n.toBigInt)})"
      case EBool(b)      => s"eval_ret (VBool $b)"
      case EStr(s)       => s"eval_ret (VStr ${cstrLit(s)})"
      case EUndef()      => "eval_ret VUndef"
      case ENull()       => "eval_ret VNull"
      case EEnum(n)      => s"eval_ret (VEnum ${strLit(n)})"
      case ENumber(n)    => s"eval_ret (VNumber ${directFloatLit(n)})"
      case EBigInt(n)    => s"eval_ret (VBigInt ${directZLit(n)})"
      case EInfinity(p)  => s"eval_ret (VInfinity $p)"
      case ECodeUnit(c)  => s"eval_ret (VCodeUnit ${c.toInt})"
      case EGrammarSymbol(n, ps) =>
        s"eval_ret (VGrammarSymbol ${strLit(n)} ${coqList(ps.map(_.toString))})"
      case EYet(_)   => "eval_throw"
      case ERef(ref) =>
        val target = fresh("target")
        s"eval_bind (${parseRef(ref, rho)}) (fun $target => eval_read_target mn $rho $target)"
      case EList(es) =>
        def go(rest: List[Expr]): String = rest match
          case Nil => "eval_ret nil"
          case head :: tail =>
            val v = fresh("v")
            val vs = fresh("vs")
            s"eval_bind (${parseOperand(head, rho)}) (fun $v => " +
              s"eval_bind (${go(tail)}) (fun $vs => eval_ret ($v :: $vs)))"
        val values = fresh("values")
        val address = fresh("address")
        s"eval_bind (${go(es)}) (fun $values => " +
          s"($address <- alloc_obj mn (OList $values);; eval_ret (VAddr $address)))"
      case ESourceText(inner) =>
        val v = fresh("value")
        val ast = fresh("ast")
        s"eval_bind (${parseOperand(inner, rho)}) (fun $v => match $v with " +
          s"| VAst _ root path => eval_bind (eval_of_option (ast_focus root path)) " +
          s"(fun $ast => eval_ret (VStr (ast_src $ast))) | _ => eval_throw end)"
      case other => fail("expr.EParse", s"unsupported operand ${other.getClass.getSimpleName}")

    private def rocqVar(v: Var): String = v match
      case local: Local => s"(VLocal ${FVExport.local(local)})"
      case Global(n)    => s"(VGlobal ${strLit(n)})"

    def ref(r: Ref, rho: String): String = r match
      case v: Var => s"Ret (TVar ${rocqVar(v)})"
      case Field(base, field) =>
        val target = fresh("target")
        val baseValue = fresh("base")
        val fieldValue = fresh("field")
        bind(
          target,
          ref(base, rho),
          bind(
            baseValue,
            s"read_target mn $rho $target",
            bind(fieldValue, expr(field, rho), s"Ret (TField $baseValue $fieldValue)"),
          ),
        )

    private def numberMath(
      lhs: Expr,
      rhs: Expr,
      rho: String,
      finish: (String, String) => String,
    ): String = {
      val lv = fresh("lv")
      val lp = fresh("lp")
      val rv = fresh("rv")
      val rp = fresh("rp")
      bind(
        lv,
        expr(lhs, rho),
        bind(
          lp,
          s"(prepare_number_math_operand $lv)?",
          bind(
            rv,
            expr(rhs, rho),
            bind(rp, s"(prepare_number_math_operand $rv)?", finish(lp, rp)),
          ),
        ),
      )
    }

    def expr(e: Expr, rho: String): String = e match
      case EMath(n) =>
        if (!n.isWhole) fail("expr.EMath", s"non-integer Math literal $n")
        s"Ret (VMath ${directZLit(n.toBigInt)})"
      case EBool(b)   => s"Ret (VBool $b)"
      case EStr(s)    => s"Ret (VStr ${cstrLit(s)})"
      case EUndef()   => "Ret VUndef"
      case ENull()    => "Ret VNull"
      case EEnum(n)   => s"Ret (VEnum ${strLit(n)})"
      case ENumber(n) => s"Ret (VNumber ${directFloatLit(n)})"
      case EBigInt(n) => s"Ret (VBigInt ${directZLit(n)})"
      case EInfinity(p) => s"Ret (VInfinity $p)"
      case ECodeUnit(c) => s"Ret (VCodeUnit ${c.toInt})"
      case ERef(r) =>
        val target = fresh("target")
        bind(target, ref(r, rho), s"read_target mn $rho $target")
      case EUnary(op, inner) =>
        val v = fresh("v")
        bind(v, expr(inner, rho), s"(eval_uop ${FVExport.rocqUOp(op)} $v)?")
      case EBinary(BOp.And, left, right) =>
        val lv = fresh("lv")
        val rv = fresh("rv")
        bind(
          lv,
          expr(left, rho),
          s"match $lv with | VBool false => Ret (VBool false) " +
            s"| VBool true => ${bind(rv, expr(right, rho), s"match $rv with | VBool b => Ret (VBool b) | _ => triggerUB end")} " +
            "| _ => triggerUB end",
        )
      case EBinary(BOp.Or, left, right) =>
        val lv = fresh("lv")
        val rv = fresh("rv")
        bind(
          lv,
          expr(left, rho),
          s"match $lv with | VBool true => Ret (VBool true) " +
            s"| VBool false => ${bind(rv, expr(right, rho), s"match $rv with | VBool b => Ret (VBool b) | _ => triggerUB end")} " +
            "| _ => triggerUB end",
        )
      case EBinary(op @ (BOp.Lt | BOp.Equal), EConvert(COp.ToMath, l), EConvert(COp.ToMath, r)) =>
        numberMath(l, r, rho, (lp, rp) =>
          s"denote_number_math_comparison mn ${FVExport.rocqBOp(op)} $lp $rp",
        )
      case EBinary(op, left, right) =>
        val lv = fresh("lv")
        val rv = fresh("rv")
        bind(
          lv,
          expr(left, rho),
          bind(
            rv,
            expr(right, rho),
            s"match host_bop_query ${FVExport.rocqBOp(op)} $lv $rv with " +
              "| Some query => (hosts <- cgetU (hosts_key mn);; " +
              "match typed_host_cache_lookup query hosts with " +
              "| Some result => Ret result | _ => triggerUB end) " +
              s"| None => (eval_bop ${FVExport.rocqBOp(op)} $lv $rv)? end",
          ),
        )
      case EClo(name, captured) =>
        val cs = fresh("captured")
        bind(cs, s"capture $rho ${coqList(captured.map(n => strLit(n.name)))}", s"Ret (VClo ${strLit(name)} $cs)")
      case ECont(name) =>
        val stack = fresh("stack")
        bind(stack, "ccallU cont_capture_sig tt", s"Ret (VCont ${strLit(name)} (capture_named_env_map $rho) $stack)")
      case EList(es) =>
        val vs = fresh("values")
        val address = fresh("address")
        bind(vs, exprList(es, rho), bind(address, s"alloc_obj mn (OList $vs)", s"Ret (VAddr $address)"))
      case ESizeOf(base) =>
        val v = fresh("value")
        bind(v, expr(base, rho), s"direct_sizeof_value mn $v")
      case ERecord(name, fields) =>
        def go(rest: List[(String, Expr)], acc: String): String = rest match
          case Nil => s"Ret $acc"
          case (field, value) :: tail =>
            val v = fresh("value")
            bind(v, expr(value, rho), go(tail, s"(fields_insert ${strLit(field)} $v $acc)"))
        val fs = fresh("fields")
        val address = fresh("address")
        bind(fs, go(fields, "nil"), bind(address, s"alloc_obj mn (ORecord ${strLit(name)} $fs)", s"Ret (VAddr $address)"))
      case EExists(reference) =>
        val target = fresh("target")
        bind(target, ref(reference, rho), s"direct_exists_value mn $rho $target")
      case ETypeOf(base) =>
        val v = fresh("value")
        bind(v, expr(base, rho), s"direct_typeof_value mn $v")
      case ETypeCheck(base, ty) =>
        val v = fresh("value")
        bind(v, expr(base, rho), s"(decision <- run_heap_query mn (ty_check_query type_check_fuel ${FVExport.rocqTy(ty)} $v);; b <- decision?;; Ret (VBool b))")
      case EYet(_) => "triggerUB"
      case EMap(_, pairs) =>
        def go(rest: List[(Expr, Expr)], acc: String): String = rest match
          case Nil => s"Ret $acc"
          case (key, value) :: tail =>
            val kv = fresh("key")
            val vv = fresh("value")
            val next = fresh("entries")
            bind(kv, expr(key, rho), bind(vv, expr(value, rho), bind(next, s"(map_insert_partial $kv $vv $acc)?", go(tail, next))))
        val entries = fresh("entries")
        val address = fresh("address")
        bind(entries, go(pairs, "nil"), bind(address, s"alloc_obj mn (OMap $entries)", s"Ret (VAddr $address)"))
      case EKeys(base, intSorted) =>
        val v = fresh("value")
        bind(v, expr(base, rho), s"direct_keys_value mn $intSorted $v")
      case ECopy(base) =>
        val v = fresh("value")
        bind(v, expr(base, rho), s"direct_copy_value mn $v")
      case EConvert(COp.ToNumber, EBinary(BOp.Add, EConvert(COp.ToMath, l), EConvert(COp.ToMath, r))) =>
        numberMath(l, r, rho, (lp, rp) => s"denote_number_math_values mn NMAdd BAdd CToNumber $lp $rp")
      case EConvert(COp.ToNumber, EBinary(BOp.Mul, EConvert(COp.ToMath, l), EConvert(COp.ToMath, r))) =>
        numberMath(l, r, rho, (lp, rp) => s"denote_number_math_values mn NMMul BMul CToNumber $lp $rp")
      case EConvert(COp.ToNumber, EBinary(BOp.Div, EConvert(COp.ToMath, l), EConvert(COp.ToMath, r))) =>
        numberMath(l, r, rho, (lp, rp) => s"denote_number_math_values mn NMDiv BDiv CToNumber $lp $rp")
      case EConvert(COp.ToApproxNumber, EBinary(BOp.Pow, EConvert(COp.ToMath, l), EConvert(COp.ToMath, r))) =>
        numberMath(l, r, rho, (lp, rp) => s"denote_number_math_values mn NMPow BPow CToApproxNumber $lp $rp")
      case EConvert(COp.ToApproxNumber, EMathOp(MOp.Sin, List(EConvert(COp.ToMath, inner)))) =>
        val v = fresh("value")
        bind(v, expr(inner, rho), s"denote_number_sin_value mn $v")
      case EConvert(COp.ToStr(radix), inner) =>
        val v = fresh("value")
        val radixTerm = radix match
          case None => s"direct_tostr_value mn $v None"
          case Some(radixExpr) =>
            val rv = fresh("radix")
            bind(rv, expr(radixExpr, rho), s"direct_tostr_value mn $v (Some $rv)")
        bind(
          v,
          expr(inner, rho),
          s"match $v with | VStr cs => Ret (VStr cs) | VNumber _ | VBigInt _ => " +
            s"$radixTerm | _ => triggerUB end",
        )
      case EConvert(op, inner) =>
        val cop = op match
          case COp.ToApproxNumber => "CToApproxNumber"
          case COp.ToNumber       => "CToNumber"
          case COp.ToBigInt       => "CToBigInt"
          case COp.ToMath         => "CToMath"
          case COp.ToCodeUnit     => "CToCodeUnit"
          case COp.ToStr(_)       => throw IllegalStateException("handled above")
        val v = fresh("value")
        bind(v, expr(inner, rho), s"denote_cop_value mn $cop $v")
      case EVariadic(op, es) =>
        val vs = fresh("values")
        val vop = op match
          case VOp.Min    => "VoMin"
          case VOp.Max    => "VoMax"
          case VOp.Concat => "VoConcat"
        bind(vs, exprList(es, rho), s"(eval_vop $vop $vs)?")
      case EMathOp(op, args) =>
        val vs = fresh("values")
        bind(vs, exprList(args, rho), s"direct_mathop_values mn ${FVExport.rocqMOp(op)} $vs")
      case EContains(list, element) =>
        val lv = fresh("list")
        val ev = fresh("elem")
        bind(lv, expr(list, rho), bind(ev, expr(element, rho), s"direct_contains_values mn $lv $ev"))
      case ETrim(inner, starting) =>
        val v = fresh("value")
        bind(v, expr(inner, rho), s"direct_trim_value $v $starting")
      case ESyntactic(name, args, rhsIdx, children) =>
        val rhs = cfg.grammar.nameMap
          .getOrElse(name, fail("expr.ESyntactic", s"unknown production $name"))
          .rhsVec
          .lift(rhsIdx)
          .getOrElse(fail("expr.ESyntactic", s"invalid RHS $name[$rhsIdx]"))
        if (children.length != rhs.nts.length)
          fail("expr.ESyntactic", s"$name[$rhsIdx] has ${children.length} children, grammar expects ${rhs.nts.length}")
        val optionalPresent = (rhs.ntsWithOptional zip children).collect {
          case ((_, true), child) => child.nonEmpty
        }
        val subIdx = optionalPresent.reverse.zipWithIndex.foldLeft(0) {
          case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
          case (acc, _)           => acc
        }
        val childNames = coqList(rhs.nts.map(nt => strLit(nt.name)))
        val layout = coqList(rhs.symbols.flatMap {
          case Terminal(term) => Some(s"(Some ${cstrLit(term)})")
          case symbol if symbol.getNt.isDefined => Some("None")
          case _                                => None
        })
        def evalChildren(rest: List[Option[Expr]]): String = rest match
          case Nil => "Ret nil"
          case None :: tail =>
            val cs = fresh("children")
            bind(cs, evalChildren(tail), s"Ret (None :: $cs)")
          case Some(child) :: tail =>
            val v = fresh("child")
            val cs = fresh("children")
            bind(v, expr(child, rho), s"match $v with | VAst _ a _ => ${bind(cs, evalChildren(tail), s"Ret (Some a :: $cs)")} | _ => triggerUB end")
        val cs = fresh("children")
        bind(cs, evalChildren(children.toList), s"direct_syntactic_values mn ${strLit(name)} ${coqList(args.map(_.toString))} ${natLit(rhsIdx)} ${natLit(subIdx)} $cs $childNames $layout")
      case EGrammarSymbol(name, params) =>
        s"Ret (VGrammarSymbol ${strLit(name)} ${coqList(params.map(_.toString))})"
      case EInstanceOf(base, target) =>
        val v = fresh("value")
        val t = fresh("target")
        bind(v, expr(base, rho), bind(t, expr(target, rho), s"Ret (eval_instanceof $v $t)"))
      case ESourceText(inner) =>
        val v = fresh("value")
        bind(v, expr(inner, rho), s"direct_source_text_value $v")
      case EParse(code, rule) =>
        if (!FVExport.parseOperandSupported(code))
          fail("expr.EParse", s"unsupported code operand ${code.getClass.getSimpleName}")
        if (!FVExport.parseOperandSupported(rule))
          fail("expr.EParse", s"unsupported rule operand ${rule.getClass.getSimpleName}")
        val cv = fresh("code")
        val rv = fresh("rule")
        bind(cv, parseOperand(code, rho), s"match $cv with | EvalThrow => alloc_parse_errors mn | EvalValue _ => " +
          s"${bind(rv, parseOperand(rule, rho), s"direct_parse_outcomes mn $cv $rv")} end")
      case ESubstring(base, from, to) =>
        val sv = fresh("string")
        val fv = fresh("from")
        val tail = to match
          case None => s"direct_substring_values $sv $fv None"
          case Some(toExpr) =>
            val tv = fresh("to")
            bind(tv, expr(toExpr, rho), s"direct_substring_values $sv $fv (Some $tv)")
        bind(sv, expr(base, rho), bind(fv, expr(from, rho), tail))
      case other => fail("expr.unhandled", other.getClass.getSimpleName)

    def inst(i: Inst, fnames: String, rho: String): String = i match
      case INop() => s"Ret ($rho, CNormal VUndef)"
      case ISeq(insts) => insts match
        case Nil => s"Ret ($rho, CNormal VUndef)"
        case head :: tail =>
          val rho1 = fresh("rho")
          val completion = fresh("completion")
          val rest = inst(ISeq(tail), fnames, rho1)
          s"('($rho1, $completion) : env * completion <- ${inst(head, fnames, rho)};; " +
            s"match $completion with | CNormal _ => $rest | CReturn v => Ret ($rho1, CReturn v) end)"
      // The sequenced value is discarded, so its type has to be pinned here
      // or elaboration leaves the ITree's return type unresolved (an [EYet]
      // operand emits a bare [triggerUB], which constrains nothing).
      case IExpr(e) =>
        seq(s"(${expr(e, rho)} : itree crisE val)", s"Ret ($rho, CNormal VUndef)")
      case ILet(lhs, e) =>
        val v = fresh("value")
        bind(v, expr(e, rho), s"Ret (env_update (LName ${strLit(lhs.name)}) $v $rho, CNormal VUndef)")
      case IAssign(reference, e) =>
        val target = fresh("target")
        val value = fresh("value")
        val rho1 = fresh("rho")
        bind(target, ref(reference, rho), bind(value, expr(e, rho), bind(rho1, s"write_target mn $rho $target $value", s"Ret ($rho1, CNormal VUndef)")))
      case IIf(cond, thenInst, elseInst, _) =>
        val cv = fresh("condition")
        bind(cv, expr(cond, rho), s"match $cv with | VBool true => ${inst(thenInst, fnames, rho)} | VBool false => ${inst(elseInst, fnames, rho)} | _ => triggerUB end")
      case IWhile(cond, body) =>
        val loopRho = fresh("rho")
        val cv = fresh("condition")
        val rho1 = fresh("rho")
        val completion = fresh("completion")
        val bodyTerm = inst(body, fnames, loopRho)
        s"ITree.iter (fun $loopRho : env => ${bind(cv, expr(cond, loopRho), s"match $cv with | VBool true => ('($rho1, $completion) : env * completion <- $bodyTerm;; match $completion with | CNormal _ => Ret (inl $rho1) | CReturn v => Ret (inr ($rho1, CReturn v)) end) | VBool false => Ret (inr ($loopRho, CNormal VUndef)) | _ => triggerUB end")}) $rho"
      case ICall(lhs, f, args) =>
        val fv = fresh("function")
        val vs = fresh("args")
        val rv = fresh("result")
        val closure = bind(vs, exprList(args, rho), bind(rv, s"ccallU (ir_sig fn) (captured, $vs)", s"Ret (env_update ${local(lhs)} $rv $rho, CNormal VUndef)"))
        val continuation = bind(vs, exprList(args, rho), s"(impossible <- ccallU cont_invoke_sig (mkContRequest fn captured $vs stack);; match impossible with end)")
        bind(fv, expr(f, rho), s"match $fv with | VClo fn captured => $closure | VCont fn captured stack => $continuation | _ => triggerUB end")
      case IReturn(e) =>
        val v = fresh("value")
        bind(v, expr(e, rho), s"Ret ($rho, CReturn $v)")
      case IAssert(EYet(_)) => s"Ret ($rho, CNormal VUndef)"
      case IAssert(e) =>
        val cv = fresh("condition")
        bind(cv, expr(e, rho), s"match $cv with | VBool true => Ret ($rho, CNormal VUndef) | _ => triggerUB end")
      case IPrint(e) =>
        val v = fresh("value")
        bind(v, expr(e, rho), seq(s"log_val $v", s"Ret ($rho, CNormal VUndef)"))
      case IPush(element, list, front) =>
        val ev = fresh("element")
        val lv = fresh("list")
        bind(ev, expr(element, rho), bind(lv, expr(list, rho), s"direct_push_values mn $rho $ev $lv $front"))
      case IPop(lhs, list, front) =>
        val lv = fresh("list")
        bind(lv, expr(list, rho), s"direct_pop_value mn $rho ${local(lhs)} $lv $front")
      case IExpand(base, field) =>
        val target = fresh("target")
        val bv = fresh("base")
        val fv = fresh("field")
        bind(target, ref(base, rho), bind(bv, s"read_target mn $rho $target", bind(fv, expr(field, rho), s"direct_expand_values mn $rho $bv $fv")))
      case IDelete(base, key) =>
        val target = fresh("target")
        val bv = fresh("base")
        val kv = fresh("key")
        bind(target, ref(base, rho), bind(bv, s"read_target mn $rho $target", bind(kv, expr(key, rho), s"direct_delete_values mn $rho $bv $kv")))
      case ISdoCall(lhs, base, method, args) =>
        val bv = fresh("base")
        bind(
          bv,
          expr(base, rho),
          s"direct_sdo_value $fnames $rho ${local(lhs)} $bv ${strLit(method)} " +
            s"(fun _ => ${exprList(args, rho)})",
        )
  }
}
