package esmeta.interpreter

import esmeta.EVAL_LOG_DIR
import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.*
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.TEST_MODE
import java.io.PrintWriter
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** extensible helper of IR interpreter with a CFG */
class Interpreter(
  val st: State,
  val log: Boolean = false,
  val logDir: String = EVAL_LOG_DIR,
  val timeLimit: Option[Int] = None,
) {
  import Interpreter.*

  /** final state */
  lazy val result: State = timeout(
    {
      while (step) {}
      if (log)
        pw.close
        println("[Interpreter] Logging finished")
      st
    },
    timeLimit,
  )

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser

  /** step */
  def step: Boolean =
    try {
      // text-based logging
      if (log)
        pw.println(st.getCursorString)
        pw.flush

      // garbage collection
      iter += 1
      if (iter % 100000 == 0) GC(st)

      // cursor
      eval(st.context.cursor)
    } catch case ReturnValue(value) => { setReturn(value); true }

  /** transition for cursors */
  def eval(cursor: Cursor): Boolean = cursor match
    case NodeCursor(node) => eval(node); true
    case ExitCursor(func) =>
      st.callStack match
        case Nil =>
          st.context.retVal.map(v => st.globals += GLOBAL_RESULT -> v)
          false
        case CallContext(retId, ctxt) :: rest =>
          val value = st.context.retVal.getOrElse(throw NoReturnValue)
          st.context = ctxt
          st.callStack = rest
          setCallResult(retId, value)
          true

  /** transition for nodes */
  def eval(node: Node): Unit =
    node match {
      case Block(_, insts, _) =>
        for (inst <- insts) eval(inst); st.context.moveNext
      case Branch(_, _, cond, thenNode, elseNode) =>
        st.context.cursor = Cursor(
          eval(cond) match {
            case Bool(true)  => thenNode
            case Bool(false) => elseNode
            case v           => throw NoBoolean(cond, v)
          },
          st.func,
        )
      case Call(_, call, _) => eval(call)
    }

  /** transition for normal instructions */
  def eval(inst: NormalInst): Unit = inst match {
    case IExpr(expr)        => eval(expr)
    case ILet(lhs, expr)    => st.context.locals += lhs -> eval(expr)
    case IAssign(ref, expr) => st.update(eval(ref), eval(expr))
    case IDelete(ref)       => st.delete(eval(ref))
    case IPush(from, to, front) =>
      eval(to) match {
        case (addr: Addr) =>
          if (front) st.prepend(addr, eval(from).toPureValue)
          else st.append(addr, eval(from).toPureValue)
        case v => throw NoAddr(to, v)
      }
    case IRemoveElem(list, elem) =>
      eval(list) match
        case (addr: Addr) => st.remove(addr, eval(elem).toPureValue)
        case v            => throw NoAddr(list, v)
    case IReturn(expr)    => throw ReturnValue(eval(expr))
    case IAssert(_: EYet) => /* skip not yet compiled assertions */
    case IAssert(expr) =>
      eval(expr) match {
        case Bool(true) =>
        case v          => throw AssertionFail(expr)
      }
    case IPrint(expr) =>
      val v = eval(expr)
      if (!TEST_MODE) println(st.getString(v))
    case INop() => /* do nothing */
  }

  /** transition for calls */
  def eval(call: CallInst): Unit = call match {
    case ICall(lhs, fexpr, args) =>
      eval(fexpr) match
        case Clo(func, captured) =>
          val vs = args.map(eval)
          val newLocals = getLocals(func.irFunc.params, vs) ++ captured
          st.callStack ::= CallContext(lhs, st.context)
          st.context = Context(func, newLocals)
        case Cont(func, captured, callStack) => {
          val needWrapped = st.context.func.isReturnComp
          val vs =
            args
              .map(eval)
              .map(v => if (needWrapped) v.wrapCompletion else v)
          val newLocals =
            getLocals(func.irFunc.params, vs, cont = true) ++ captured
          st.callStack = callStack.map(_.copied)
          st.context = Context(func, newLocals)
        }
        case v => throw NoFunc(fexpr, v)
    case IMethodCall(lhs, base, method, args) =>
      val bv = st(eval(base))
      // TODO do not explicitly store methods in object but use a type model
      // when accessing methods
      st(bv, Str(method)) match
        case Clo(func, _) =>
          val vs = args.map(eval)
          val newLocals = getLocals(func.irFunc.params, bv :: vs)
          st.callStack ::= CallContext(lhs, st.context)
          st.context = Context(func, newLocals)
        case v => throw NoFunc(call.fexpr, v)
    case ISdoCall(lhs, base, method, args) =>
      eval(base).asAst match
        case syn: Syntactic =>
          getSDO((syn, method)) match
            case Some((ast0, sdo)) =>
              val vs = args.map(eval)
              val newLocals =
                getLocals(sdo.irFunc.params, AstValue(ast0) :: vs)
              st.callStack ::= CallContext(lhs, st.context)
              st.context = Context(sdo, newLocals)
            case None => throw InvalidAstProp(syn, Str(method))
        case lex: Lexical =>
          setCallResult(lhs, Interpreter.eval(lex, method))
  }

  /** transition for expresssions */
  def eval(expr: Expr): Value = expr match {
    case EComp(tyExpr, valExpr, tgtExpr) =>
      val y = eval(tyExpr)
      val t = eval(tgtExpr)
      val v = eval(valExpr).toPureValue
      (y, t) match
        case (y: Const, Str(t))      => Comp(y, v, Some(t))
        case (y: Const, CONST_EMPTY) => Comp(y, v, None)
        case (y: Const, t)           => throw InvalidCompTarget(y)
        case (y, t)                  => throw InvalidCompType(t)
    case EIsCompletion(expr) =>
      Bool(eval(expr).isCompletion)
    case EReturnIfAbrupt(ERef(ref), check) =>
      val refV = eval(ref)
      val value = returnIfAbrupt(st(refV), check)
      st.update(refV, value)
      value
    case EReturnIfAbrupt(expr, check) =>
      returnIfAbrupt(eval(expr), check)
    case EPop(list, front) =>
      eval(list) match
        case (addr: Addr) => st.pop(addr, front)
        case v            => throw NoAddr(list, v)
    case EParse(code, rule) =>
      val (str, args, locOpt) = eval(code) match
        case Str(s) => (s, List(), None)
        case AstValue(syn: Syntactic) =>
          (syn.toString(grammar = Some(grammar)), syn.args, syn.loc)
        case AstValue(lex: Lexical) => (lex.str, List(), lex.loc)
        case v                      => throw InvalidParseSource(code, v)
      try {
        (str, eval(rule), st.sourceText, st.cachedAst) match
          // optimize the initial parsing using the given cached AST
          case (x, Grammar("Script", Nil), Some(y), Some(ast)) if x == y =>
            AstValue(ast)
          case (x, Grammar(name, params), _, _) =>
            val ast =
              esParser(name, if (params.isEmpty) args else params).from(x)
            // TODO handle span of re-parsed ast
            ast.clearLoc
            ast.setChildLoc(locOpt)
            AstValue(ast)
          case (_, r, _, _) => throw NoGrammar(rule, r)
      } catch {
        case _: Throwable => st.allocList(Nil) // NOTE: throw a List of errors
      }
    case EGrammar(name, params) => Grammar(name, params)
    case ESourceText(expr) =>
      val ast = eval(expr).asAst
      // XXX fix last space in ECMAScript stringifier
      Str(ast.toString(grammar = Some(grammar)).trim)
    case EGetChildren(kindOpt, ast) =>
      val kOpt = kindOpt.map(kind =>
        eval(kind) match
          case Grammar(name, _) => name
          case v                => throw NoGrammar(kind, v),
      )
      val a = eval(ast).asAst
      (a, kOpt) match
        case (_, Some(k)) => st.allocList(a.getChildren(k).map(AstValue(_)))
        case (syn: Syntactic, None) =>
          st.allocList(syn.children.flatten.map(AstValue(_)))
        case _ => throw InvalidASTChildren(a)
    case EYet(msg) =>
      throw NotSupported(msg)
    case EContains(list, elem, field) =>
      val l = eval(list).getList(list, st)
      val e = eval(elem)
      Bool(field match {
        case Some((_, f)) => l.values.exists(x => st(PropValue(x, Str(f))) == e)
        case None         => l.values.contains(e)
      })
    case ESubstring(expr, from, to) =>
      val s = eval(expr).asStr
      val f = eval(from).asInt
      Str(to.fold(s.substring(f))(eval(_) match
        case Math(n) if s.length < n => s.substring(f)
        case v                       => s.substring(f, v.asInt),
      ))
    case ERef(ref) =>
      st(eval(ref))
    case EUnary(uop, expr) =>
      val x = eval(expr)
      Interpreter.eval(uop, x)
    case EBinary(BOp.And, left, right) => shortCircuit(BOp.And, left, right)
    case EBinary(BOp.Or, left, right)  => shortCircuit(BOp.Or, left, right)
    case EBinary(BOp.Eq, ERef(ref), EAbsent) => Bool(!st.exists(eval(ref)))
    case EBinary(bop, left, right) =>
      val l = eval(left)
      val r = eval(right)
      Interpreter.eval(bop, l, r)
    case EClamp(target, lower, upper) =>
      val tv = eval(target)
      val lv = eval(lower)
      val uv = eval(upper)
      (tv, lv, uv) match {
        // mathematical value operations
        case (Math(t), Math(l), Math(u)) =>
          if (t < l) Math(l)
          else if (t > u) Math(u)
          else Math(t)

        // extended mathematical value operations
        case (POS_INF, Math(l), Math(u)) => Math(u)
        case (NEG_INF, Math(l), Math(u)) => Math(l)

        case (_, _, _) =>
          throw InvalidClampOp(tv, lv, uv)
      }
    case EVariadic(vop, exprs) =>
      val vs = for (e <- exprs) yield eval(e).toPureValue
      Interpreter.eval(vop, vs)
    case EConvert(cop, expr) =>
      import COp.*
      (eval(expr), cop) match {
        // code unit
        case (CodeUnit(c), ToMath) => Math(BigDecimal.exact(c.toInt))
        // mathematical value
        case (Math(n), ToNumber) => Number(n.toDouble)
        case (Math(n), ToBigInt) => BigInt(n.toBigInt)
        case (Math(n), ToMath)   => Math(n)
        // string
        case (Str(s), ToNumber) => Number(ESValueParser.str2Number(s))
        case (Str(s), ToBigInt) => ESValueParser.str2bigInt(s)
        case (Str(s), _: ToStr) => Str(s)
        // numbers
        case (Number(d), ToMath) => Math(BigDecimal.exact(d))
        case (Number(d), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(toStringHelper(d, radix))
        case (Number(d), ToNumber) => Number(d)
        case (Number(n), ToBigInt) => BigInt(BigDecimal.exact(n).toBigInt)
        // big integer
        case (BigInt(n), ToMath) => Math(BigDecimal.exact(n))
        case (BigInt(n), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(n.toString(radix))
        case (BigInt(n), ToBigInt) => BigInt(n)
        // invalid cases
        case (v, cop) => throw InvalidConversion(cop, expr, v)
      }
    case ETypeOf(base) =>
      Str(eval(base) match
        case n: Number => "Number"
        case b: BigInt => "BigInt"
        case s: Str    => "String"
        case b: Bool   => "Boolean"
        case Undef     => "Undefined"
        case Null      => "Null"
        case addr: Addr =>
          st(addr) match
            case m: MapObj =>
              if (tyModel.isSubTy(m.ty, "Object")) "Object"
              else m.ty
            case _: ListObj   => "List"
            case _: SymbolObj => "Symbol"
            case v            => ???
        case v => ???,
      )
    case ETypeCheck(expr, tyExpr) =>
      val v = eval(expr)
      val tyName = eval(tyExpr) match
        case Str(s)        => s
        case Grammar(s, _) => s
        case v             => throw InvalidTypeExpr(expr, v)
      Bool(v match
        case _: Number => tyName == "Number"
        case _: BigInt => tyName == "BigInt"
        case _: Str    => tyName == "String"
        case _: Bool   => tyName == "Boolean"
        case _: Const  => tyName == "Constant"
        case _: Comp   => tyName == "CompletionRecord"
        case Undef     => tyName == "Undefined"
        case Null      => tyName == "Null"
        case AstValue(ast) =>
          tyName == "ParseNode" || (ast.types contains tyName)
        case _: Clo => tyName == "AbstractClosure"
        case addr: Addr =>
          st(addr) match
            case m: MapObj    => tyModel.isSubTy(m.ty, tyName)
            case _: ListObj   => tyName == "List"
            case _: SymbolObj => tyName == "Symbol"
            case _            => ???
        case v => ???,
      )
    case EClo(fname, captured) =>
      val func = cfg.fnameMap.getOrElse(fname, throw UnknownFunc(fname))
      Clo(func, Map.from(captured.map(x => x -> st(x))))
    case ECont(fname) =>
      val func = cfg.fnameMap.getOrElse(fname, throw UnknownFunc(fname))
      val captured = st.context.locals.collect { case (x: Name, v) => x -> v }
      Cont(func, Map.from(captured), st.callStack)
    case ESyntactic(name, args, rhsIdx, children) =>
      val asts = children.map(childOpt =>
        childOpt.map(child =>
          eval(child) match {
            case AstValue(ast) => ast
            case v             => throw NoAst(child, v)
          },
        ),
      )
      AstValue(Syntactic(name, args, rhsIdx, asts))
    case ELexical(name, expr) =>
      val str = eval(expr).asStr
      AstValue(Lexical(name, str))
    case EMap("Completion", props) =>
      val map = (for {
        (kexpr, vexpr) <- props
        k = eval(kexpr)
        v = eval(vexpr)
      } yield k -> v).toMap
      (
        map.get(Str("Type")),
        map.get(Str("Value")),
        map.get(Str("Target")),
      ) match
        case (Some(ty: Const), Some(value), Some(target)) =>
          val targetOpt = target match
            case Str(target) => Some(target)
            case CONST_EMPTY => None
            case v           => throw InvalidCompTarget(v)
          Comp(ty, value.toPureValue, targetOpt)
        case _ => throw InvalidComp
    case EMap(tname, props) =>
      val addr = st.allocMap(tname)
      for ((kexpr, vexpr) <- props)
        val k = eval(kexpr).toPureValue
        val v = eval(vexpr)
        st.update(addr, k, v)
      addr
    case EList(exprs) =>
      st.allocList(exprs.map(expr => eval(expr).toPureValue))
    case EListConcat(exprs) =>
      val ls = exprs.map(e => eval(e).getList(e, st).values).flatten
      st.allocList(ls)
    case ESymbol(desc) =>
      eval(desc) match
        case (str: Str) => st.allocSymbol(str)
        case Undef      => st.allocSymbol(Undef)
        case v          => throw NoString(desc, v)
    case ECopy(obj) =>
      eval(obj) match
        case addr: Addr => st.copyObj(addr)
        case v          => throw NoAddr(obj, v)
    case EKeys(map, intSorted) =>
      eval(map) match
        case addr: Addr => st.keys(addr, intSorted)
        case v          => throw NoAddr(map, v)
    case EDuplicated(expr) =>
      val vs = eval(expr).getList(expr, st).values
      Bool(vs.toSet.size != vs.length)
    case EIsArrayIndex(expr) =>
      eval(expr) match
        case Str(s) =>
          val d = ESValueParser.str2Number(s)
          val ds = toStringHelper(d)
          val UPPER = (1L << 32) - 1
          val l = d.toLong
          Bool(ds == s && 0 <= l && d == l && l < UPPER)
        case _ => Bool(false)
    case EMathVal(n)           => Math(n)
    case ENumber(n) if n.isNaN => Number(Double.NaN)
    case ENumber(n)            => Number(n)
    case EBigInt(n)            => BigInt(n)
    case EStr(str)             => Str(str)
    case EBool(b)              => Bool(b)
    case EUndef                => Undef
    case ENull                 => Null
    case EAbsent               => Absent
    case EConst(name)          => Const(name)
    case ECodeUnit(c)          => CodeUnit(c)
  }

  /** short circuit evaluation */
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value =
    val l = eval(left)
    (bop, l) match
      case (BOp.And, Bool(false)) => Bool(false)
      case (BOp.Or, Bool(true))   => Bool(true)
      case _ =>
        val r = eval(right)
        Interpreter.eval(bop, l, r)

  /** get initial local variables */
  def getLocals(
    params: List[Param],
    args: List[Value],
    cont: Boolean = false,
  ): MMap[Local, Value] = {
    val map = MMap[Local, Value]()
    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
      case (Nil, Nil)                           =>
      case (Param(lhs, _, optional) :: pl, Nil) =>
        // TODO parameter type check
        if (optional) {
          map += lhs -> Absent
          aux(pl, Nil)
        } else RemainingParams(ps)
      case (Nil, args) =>
        // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
        if (!cont) throw RemainingArgs(args)
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** helper for return-if-abrupt cases */
  def returnIfAbrupt(value: Value, check: Boolean): Value = value match
    case NormalComp(value) => value
    case comp: Comp =>
      if (check) throw ReturnValue(value)
      else throw UncheckedAbrupt(comp)
    case pure: PureValue => pure // XXX remove?

  /** transition for references */
  def eval(ref: Ref): RefValue = ref match
    case x: Id => IdValue(x)
    case Prop(ref, expr) =>
      var base = st(eval(ref))
      val p = eval(expr)
      PropValue(base, p.toPureValue)

  /** set return value and move to the exit node */
  def setReturn(value: Value): Unit =
    // set type map
    (value, setTypeMap.get(st.context.name)) match
      case (addr: Addr, Some(tname))             => st.setType(addr, tname)
      case (NormalComp(addr: Addr), Some(tname)) => st.setType(addr, tname)
      case _                                     => /* do nothing */
    st.context.retVal = Some(
      // wrap completion by conditions specified in
      // [5.2.3.5 Implicit Normal Completion]
      // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
      if (st.context.func.isReturnComp) value.wrapCompletion else value,
    )
    st.context.cursor = ExitCursor(st.func)

  /** define call result to state and move to next */
  def setCallResult(id: Id, value: Value): Unit =
    st.define(id, value)
    st.context.moveNext

  /** sdo with default case */
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
    "ContainsArguments",
  )

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  /** control flow graphs */
  private given cfg: CFG = st.cfg

  /** type model */
  private def tyModel = cfg.tyModel

  /** grammar */
  private def grammar = cfg.grammar

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    println(s"[Interpreter] Logging into $logDir...")
    mkdir(logDir)
    getPrintWriter(s"$logDir/log")

  /** get syntax-directed operation(SDO) */
  private val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
    case (ast, operation) =>
      val fnameMap = cfg.fnameMap
      ast.chains.foldLeft[Option[(Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname) match
            case Some(sdo) => Some(ast0, sdo)
            case None if defaultCases contains operation =>
              Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
            case _ => None
        case (res: Some[_], _) => res
      }
  }

  /** get sub index of parsed Ast */
  private val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }
}

/** IR interpreter with a CFG */
object Interpreter {
  def apply(
    st: State,
    log: Boolean = false,
    logDir: String = EVAL_LOG_DIR,
    timeLimit: Option[Int] = None,
  ): State = new Interpreter(st, log, logDir, timeLimit).result

  // type update algorithms
  val setTypeMap: Map[String, String] = Map(
    "OrdinaryFunctionCreate" -> "ECMAScriptFunctionObject",
    "ArrayCreate" -> "ArrayExoticObject",
    "OrdinaryObjectCreate" -> "OrdinaryObject",
  )

  /** transition for lexical SDO */
  def eval(lex: Lexical, sdoName: String): PureValue = {
    val Lexical(name, str) = lex
    (name, sdoName) match {
      case (
            "IdentifierName \\ (ReservedWord)" | "IdentifierName",
            "StringValue",
          ) =>
        Str(ESValueParser.parseIdentifier(str))
      case ("PrivateIdentifier", "StringValue") =>
        Str("#" + ESValueParser.parseIdentifier(str.substring(1)))
      // TODO handle numeric seperator in ESValueParser
      case ("NumericLiteral", "MV" | "NumericValue") =>
        ESValueParser.parseNumber(str.replaceAll("_", ""))
      case ("StringLiteral", "SV" | "StringValue") =>
        Str(ESValueParser.parseString(str))
      case ("NoSubstitutionTemplate", "TV") =>
        ESValueParser.parseTVNoSubstitutionTemplate(str)
      case ("TemplateHead", "TV") =>
        ESValueParser.parseTVTemplateHead(str)
      case ("TemplateMiddle", "TV") =>
        ESValueParser.parseTVTemplateMiddle(str)
      case ("TemplateTail", "TV") =>
        ESValueParser.parseTVTemplateTail(str)
      case ("NoSubstitutionTemplate", "TRV") =>
        Str(ESValueParser.parseTRVNoSubstitutionTemplate(str))
      case ("TemplateHead", "TRV") =>
        Str(ESValueParser.parseTRVTemplateHead(str))
      case ("TemplateMiddle", "TRV") =>
        Str(ESValueParser.parseTRVTemplateMiddle(str))
      case ("TemplateTail", "TRV") =>
        Str(ESValueParser.parseTRVTemplateTail(str))
      case (_, "Contains") => Bool(false)
      case ("RegularExpressionLiteral", name) =>
        throw NotSupported(s"RegularExpressionLiteral.$sdoName")
      case _ =>
        throw InvalidAstProp(lex, Str(sdoName))
    }
  }

  /** transition for unary operators */
  def eval(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      // mathematic values
      case (Abs, Math(n))                    => Math(n.abs)
      case (Floor, Math(n)) if n.isValidLong => Math(n)
      case (Floor, Math(n)) => Math(n - (n % 1) - (if (n < 0) 1 else 0))
      // numeric values
      case (Neg, Number(n)) => Number(-n)
      case (Neg, Math(n))   => Math(-n)
      case (Neg, BigInt(b)) => BigInt(-b)
      // boolean
      case (Not, Bool(b)) => Bool(!b)
      // bitwise
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, Number(n)) => Number(~(n.toInt))
      case (BNot, BigInt(b)) => BigInt(~b)
      case (_, value)        => throw InvalidUnaryOp(uop, value)

  /** transition for binary operators */
  def eval(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    (bop, left, right) match {
      // double operations
      case (Plus, Number(l), Number(r)) => Number(l + r)
      case (Sub, Number(l), Number(r))  => Number(l - r)
      case (Mul, Number(l), Number(r))  => Number(l * r)
      case (Pow, Number(l), Number(r))  => Number(math.pow(l, r))
      case (Div, Number(l), Number(r))  => Number(l / r)
      case (Mod, Number(l), Number(r))  => Number(l % r)
      case (UMod, Number(l), Number(r)) => Number(l %% r)
      case (Lt, Number(l), Number(r)) if (l equals -0.0) && (r equals 0.0) =>
        Bool(true)
      case (Lt, Number(l), Number(r)) => Bool(l < r)

      // mathematical value operations
      case (Plus, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r))  => Math(l - r)
      case (Mul, Math(l), Math(r))  => Math(l * r)
      case (Div, Math(l), Math(r))  => Math(l / r)
      case (Mod, Math(l), Math(r)) =>
        val m = l % r
        Math(if (m * r) < 0 then r + m else m)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) =>
        Math(math.pow(l.toDouble, r.toDouble))
      // TODO consider 2's complement 32-bit strings
      case (BAnd, Math(l), Math(r)) => Math(l.toLong & r.toLong)
      case (BOr, Math(l), Math(r))  => Math(l.toLong | r.toLong)
      case (BXOr, Math(l), Math(r)) => Math(l.toLong ^ r.toLong)
      case (LShift, Math(l), Math(r)) =>
        Math((l.toInt << r.toInt).toLong)
      case (SRShift, Math(l), Math(r)) =>
        Math((l.toInt >> r.toInt).toLong)
      case (URShift, Math(l), Math(r)) =>
        Math((l.toLong << 32) >>> (32 + (r.toLong % 32)))
      case (Lt, Math(l), Math(r)) => Bool(l < r)

      // extended mathematical value operations
      case (Lt, POS_INF, Math(r)) => Bool(false)
      case (Lt, Math(r), POS_INF) => Bool(true)
      case (Lt, NEG_INF, Math(r)) => Bool(true)
      case (Lt, Math(r), NEG_INF) => Bool(false)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Number(l), Number(r))     => Bool(l equals r)
      case (Eq, AstValue(l), AstValue(r)) => Bool(l eq r)
      case (Eq, l, r)                     => Bool(l == r)

      // numeric equality operations
      case (Equal, Math(l), Math(r))     => Bool(l == r)
      case (Equal, Number(l), Number(r)) => Bool(l == r)
      case (Equal, BigInt(l), BigInt(r)) => Bool(l == r)

      // big integers
      case (Plus, BigInt(l), BigInt(r))    => BigInt(l + r)
      case (LShift, BigInt(l), BigInt(r))  => BigInt(l << r.toInt)
      case (SRShift, BigInt(l), BigInt(r)) => BigInt(l >> r.toInt)
      case (Sub, BigInt(l), BigInt(r))     => BigInt(l - r)
      case (Mul, BigInt(l), BigInt(r))     => BigInt(l * r)
      case (Div, BigInt(l), BigInt(r))     => BigInt(l / r)
      case (Mod, BigInt(l), BigInt(r))     => BigInt(l % r)
      case (UMod, BigInt(l), BigInt(r))    => BigInt(l %% r)
      case (Lt, BigInt(l), BigInt(r))      => Bool(l < r)
      case (BAnd, BigInt(l), BigInt(r))    => BigInt(l & r)
      case (BOr, BigInt(l), BigInt(r))     => BigInt(l | r)
      case (BXOr, BigInt(l), BigInt(r))    => BigInt(l ^ r)
      case (Pow, BigInt(l), BigInt(r))     => BigInt(l.pow(r.toInt))

      case (_, lval, rval) => throw InvalidBinaryOp(bop, lval, rval)
    }

  /** transition for variadic operators */
  def eval(vop: VOp, vs: List[PureValue]): PureValue =
    import VOp.*
    if (vs.isEmpty) throw InvalidVariadicOp(vop)
    vop match
      case Min =>
        if (vs.contains(NEG_INF)) NEG_INF
        else {
          val filtered = vs.filter(_ != POS_INF)
          if (filtered.isEmpty) POS_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
      case Max =>
        if (vs.contains(POS_INF)) POS_INF
        else {
          val filtered = vs.filter(_ != NEG_INF)
          if (filtered.isEmpty) NEG_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
        vopEval(_.asMath, _ max _, Math(_), vs)
      case Concat => vopEval(_.asStr, _ + _, Str(_), vs)

  /** helpers for make transition for variadic operators */
  private def vopEval[T](
    f: PureValue => T,
    op: (T, T) => T,
    g: T => PureValue,
    vs: List[PureValue],
  ) = g(vs.map(f).reduce(op))
}
