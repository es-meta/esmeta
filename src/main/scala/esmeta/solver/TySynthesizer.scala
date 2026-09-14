package esmeta.solver

import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.{Block, CFG}
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.solver.Solver.Template
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}
import scala.util.*

class TySynthesizer(cfg: CFG, val tychecker: TyChecker) {
  import TySynthesizer.*

  /** prepare manual observations */
  def prepare: Unit = { manualEntries.size; () }

  /** synthesize a value using constants from the specification */
  def synthesize(ty: ValueTy)(using
    checkDeadline: () => Unit = () => (),
  ): Option[String] = {
    checkDeadline()
    val valueTy = ty && ESValueT
    if (valueTy.isBottom) None
    else
      firstSuccess(
        List(
          () => fromDirect(valueTy),
          () => fromRecord(valueTy),
          () => fromTemplate(valueTy),
        ),
      )
  }

  private def firstSuccess[A](choices: List[() => Option[A]]): Option[A] =
    shuffle(choices).iterator.flatMap(_()).nextOption()

  private val cachedManuals = TrieMap[ValueTy, List[String]]()

  private def fromDirect(ty: ValueTy): Option[String] = {
    val values = (primitives(ty) ++ manuals(ty)).distinct
    Option.when(values.nonEmpty)(choose(values))
  }

  // exact values and primitive seeds
  private def primitives(ty: ValueTy): List[String] =
    val numberSet = ty.number.toNumberSet
    val numbers = numberSet.fold(Nil) { set =>
      set.toList.sortBy(n => (n.isNaN, n.double)).map(numberLit)
    }
    val exact = numbers ++
      (ty.str match
        case Fin(set) => set.toList.map(str => s"\"${normStr(str)}\"")
        case Inf      => Nil
      ) ++
      ty.bool.set.toList.sorted.map(b => if (b) "true" else "false") ++
      (if (ty.undef) List("undefined") else Nil) ++
      (if (ty.nullv) List("null") else Nil)
    val examples =
      (if (numberSet.isEmpty)
         (literals.numbers ++ List(0, 1, -1).map(n => Number(n)))
           .filter(ty.number.contains)
           .map(numberLit)
       else Nil) ++
      (if (ty.bigInt)
         (literals.bigInts :+ SBigInt(0)).map(n => s"${n}n")
       else Nil) ++
      (ty.str match
        case Inf =>
          (literals.strings :+ "")
            .map(s => "\"" + normStr(s) + "\"")
        case _ => Nil
      )
    exact ++ examples

  private def numberLit(n: Number): String =
    val d = n.double
    if (d.isNaN) "NaN"
    else if (d.isPosInfinity) "Infinity"
    else if (d.isNegInfinity) "-Infinity"
    else if (d == 0 && 1 / d < 0) "-0"
    else if (d.isWhole && d.abs <= 9007199254740991.0) d.toLong.toString
    else d.toString

  // synthesize structural object requirements
  private def fromRecord(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record match {
    case RecordTy.Elem(map, ObjShape(props, call, construct))
        if props.nonEmpty =>
      val ordered = props.toList.sortBy { case (prop, _) => propKey(prop) }
      def objectLiteral(): Option[String] =
        ordered
          .foldLeft(Option(List.empty[String])) {
            case (fields, (prop, desc)) =>
              fields.flatMap { fs =>
                val key = propKey(prop)
                val field =
                  if (desc.getExc) Some(s"get $key() { throw 0; }")
                  else if (desc.setExc) Some(s"set $key(_) { throw 0; }")
                  else synthesize(desc.ty).map(v => s"$key: $v")
                field.map(_ :: fs)
              }
          }
          .map(_.reverse.mkString("{ ", ", ", " }"))
      if (isPlainObject(ty) && !call.exists && !construct.exists)
        objectLiteral()
      else {
        val baseTy = ty.copied(record =
          RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
        )
        val overlay = () => {
          // preserve existing attributes and unspecified accessors
          val updates = ordered
            .map { (prop, desc) =>
              val key = propExpr(prop)
              val field =
                if (desc.getExc) "get" else if (desc.setExc) "set" else "value"
              s"[$key]: Object.getOwnPropertyDescriptor(o, $key) ? " +
              s"{ $field: ds[$key].$field } : ds[$key]"
            }
            .mkString("{ ", ", ", " }")
          for {
            base <- synthesize(baseTy)
            obj <- objectLiteral()
          } yield s"((o, ds) => Object.defineProperties(o, $updates))" +
          s"($base, Object.getOwnPropertyDescriptors($obj))"
        }
        val proxy = ordered match {
          case (prop, desc) :: Nil =>
            List(() => {
              val key = propExpr(prop)
              for {
                base <- synthesize(baseTy)
                handler <-
                  if (desc.getExc)
                    Some(
                      s"get(t, p, r) { if (p === $key) throw 0; return Reflect.get(t, p, r); }",
                    )
                  else if (desc.setExc)
                    Some(
                      s"set(t, p, v, r) { if (p === $key) throw 0; return Reflect.set(t, p, v, r); }",
                    )
                  else
                    synthesize(desc.ty).map(v =>
                      s"get(t, p, r) { if (p === $key) return $v; return Reflect.get(t, p, r); }",
                    )
              } yield s"new Proxy($base, { $handler })"
            })
          case _ => Nil
        }
        firstSuccess(overlay :: proxy)
      }
    case _ => firstSuccess(List(() => fromConstruct(ty), () => fromCall(ty)))
  }

  private def fromConstruct(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record.construct match {
    case ConstructDesc.Elem(exc, ret) =>
      if (exc) Some("function() { throw 0; }")
      else synthesize(ret).map(v => s"function() { return $v; }")
    case ConstructDesc.Top => None
  }

  private def fromCall(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record.call match {
    case CallDesc.Elem(exc, ret) =>
      val isCtor = ty <= ConstructorT
      if (exc)
        Some(if (isCtor) "function() { throw 0; }" else "() => { throw 0; }")
      else
        synthesize(ret).map { value =>
          if (isCtor) s"function() { return $value; }" else s"() => ($value)"
        }
    case CallDesc.Top => None
  }

  private def isPlainObject(ty: ValueTy): Boolean = ty.record match
    case RecordTy.Elem(map, _) =>
      ObjectT ⊑ ty.copied(record = RecordTy.Elem(map))
    case _ => ObjectT ⊑ ty

  private def propExpr(prop: Property): String = prop match
    case Property.PStr(str) => s"\"${normStr(str)}\""
    case Property.PSym(sym) => s"Symbol.$sym"

  private def propKey(prop: Property): String = s"[${propExpr(prop)}]"

  private def manuals(ty: ValueTy): List[String] =
    if (ty.isBottom) Nil
    else
      cachedManuals.getOrElseUpdate(
        ty,
        manualEntries.filter(_.ty <= ty).map(_.expr).distinct,
      )

  private def fromTemplate(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] =
    if (!searchFields(ty).exists(targetCandidates.contains(_))) None
    else
      derive(ty).iterator
        .flatMap(_.instantiate(ty => synthesize(ty)))
        .nextOption()

  // derive templates at the candidate entries' returns
  private def derive(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): LazyList[Template] =
    val targets = searchFields(ty).toList
      .flatMap(field => targetCandidates.getOrElse(field, Nil))
    val entries = SymInterp
      .sortedEntries(targets)(using cfg)
      .filter(Solver.funcAccessExpr(_).nonEmpty)
    LazyList.from(entries).flatMap { entry =>
      LazyList.from(shuffle(entry.exits.toList)).flatMap {
        case block: Block
            if block.insts.lastOption.exists(_.isInstanceOf[IReturn]) =>
          val IReturn(expr) = block.insts.last: @unchecked
          val interp = new SymInterp(
            this.tychecker,
            entry,
            block,
            checkDeadline = checkDeadline,
          )
          val tychecker: interp.tychecker.type = interp.tychecker
          import tychecker.{cfg => _, *}
          given NodePoint[?] = NodePoint(entry, block, emptyView)
          val required = NormalT(ty)
          for {
            config <- LazyList
              .unfold(())(_ => interp.nextCandidate.map(_ -> ()))
            st = block.insts.init.foldLeft(config.state) {
              case (st, _) if st.isBottom => st
              case (st, inst)             => transfer.transfer(inst)(st)
            }
            if !st.isBottom
            (value, retSt) = transfer.transfer(expr)(st)
            if !retSt.isBottom
            symty = value.onlySym(using retSt).symty
            prop <- inputConstraints(tychecker)(symty, required, retSt)
            refinedSt = transfer.refine(prop)(retSt)
            if !refinedSt.isBottom && !symty.ty(using refinedSt).isBottom
            if symty.ty(using refinedSt) ⊑ required
            template <- Solver.getTemplate(tychecker)(entry, refinedSt)
          } yield template
        case _ => LazyList.empty
      }
    }

  private def inputConstraints(tychecker: TyChecker)(
    symty: tychecker.SymTy,
    ty: ValueTy,
    st: tychecker.AbsState,
  ): Option[tychecker.TypeProp] =
    import tychecker.*, SymTy.*
    given AbsState = st
    if (symty.ty ⊑ ty) Some(TypeProp.Top)
    else
      symty match
        case ref: SymRef => transfer.toBase(ref, ty).map(TypeProp(_))
        case SRecord(_, fields) =>
          explicitFields(ty).toList.sorted
            .foldLeft(Option(TypeProp.Top)) {
              case (acc, field) =>
                acc.flatMap { prop =>
                  val required = ty.record(field).value
                  if (symty.ty.record(field).value ⊑ required)
                    Some(prop)
                  else
                    for {
                      fieldSymty <- fields.get(field)
                      next <- inputConstraints(tychecker)(
                        fieldSymty,
                        required,
                        st,
                      )
                    } yield prop && next
                }
            }
        case STy(_) => None

  private def explicitFields(ty: ValueTy): Set[String] = ty.record match
    case RecordTy.Elem(map, _) => map.values.flatMap(_.map.keySet).toSet
    case _                     => Set.empty

  // select fields for target search
  private def searchFields(ty: ValueTy): Set[String] =
    val explicit = explicitFields(ty)
      .filter(field => !ty.record(field).value.isBottom)
    if (explicit.exists(targetCandidates.contains(_))) explicit
    else {
      // recover slots encoded by record names (e.g., NumberObject)
      val declared = ty.record match
        case RecordTy.Elem(map, _) =>
          map.keys.flatMap { name =>
            val model = ManualInfo.tyModel
            val base = model.baseOf(name)
            val common = model.upperFieldsOf(base).keySet
            model
              .diffOf(base, name)
              .toList
              .flatMap(_.map.collect {
                case (field, binding) if !binding.absent && !common(field) =>
                  field
              })
          }.toSet
        case _ => Set.empty[String]
      declared.filter(field => !ty.record(field).value.isBottom)
    }

  private def fieldWrites(inst: NormalInst): List[(Local, String, Expr)] =
    inst match
      case IAssign(Field(local: Local, EStr(field)), value) =>
        List((local, field, value))
      case ILet(local, ERecord(_, fields)) =>
        fields.map((field, value) => (local, field, value))
      case IAssign(local: Local, ERecord(_, fields)) =>
        fields.map((field, value) => (local, field, value))
      case _ => Nil

  // keep all writes to fields with a potentially symbolic source
  private lazy val targetCandidates: Map[String, List[Block]] =
    val writes = (for {
      func <- cfg.funcs
      block <- func.nodes.collect { case block: Block => block }
      (_, field, _) <- block.insts.flatMap(fieldWrites)
    } yield field -> block).toList.groupMap(_._1)(_._2)
    writes.filter { (field, blocks) =>
      blocks.exists { block =>
        block.insts.iterator.flatMap(fieldWrites).exists {
          case (_, `field`, value) =>
            value match
              case _: LiteralExpr | EClo(_, Nil) => false
              case _                             => true
          case _ => false
        }
      }
    }

  // collect specification constants once per synthesizer
  private lazy val literals: Literals =
    // separate NaN, infinities, and -0 from decimal folding
    val decimals = MMap[BigDecimal, Int]().withDefaultValue(0)
    val doubles = MMap[Double, Int]().withDefaultValue(0)
    val bigInts = MMap[SBigInt, Int]().withDefaultValue(0)
    val strings = MMap[String, Int]().withDefaultValue(0)
    def bump[T](to: MMap[T, Int], key: T): Unit = to(key) = to(key) + 1
    val walker = new UnitWalker {
      override def walk(expr: Expr): Unit = expr match
        case EStr(str)  => bump(strings, str)
        case EBigInt(n) => bump(bigInts, n)
        case ENumber(d) if !d.isFinite || (d == 0 && 1 / d < 0) =>
          bump(doubles, d)
        case _ =>
          foldLiterals(expr) match
            case Some(n) => bump(decimals, n)
            case None    => super.walk(expr)
      override def walk(ref: Ref): Unit = ref match
        case Field(base, _) => walk(base)
        case _              => super.walk(ref)
    }
    walker.walk(cfg.program)
    def ranked[T](from: MMap[T, Int])(using Ordering[T]): List[T] =
      from.toList.sortBy((lit, n) => (-n, lit)).map(_._1)
    val numbers = ranked(decimals).map(n => Number(n.toDouble)) ++
      ranked(doubles).map(Number(_))
    Literals(numbers, ranked(bigInts), ranked(strings))

  private def foldLiterals(expr: Expr): Option[BigDecimal] = expr match
    case EMath(n)   => Some(n)
    case ENumber(d) => Option.when(!d.isNaN && !d.isInfinite)(BigDecimal(d))
    case EUnary(UOp.Neg, e) => foldLiterals(e).map(-_)
    case EBinary(bop, left, right) =>
      for {
        x <- foldLiterals(left)
        y <- foldLiterals(right)
        z <- bop match
          case BOp.Add => Some(x + y)
          case BOp.Sub => Some(x - y)
          case BOp.Mul => Some(x * y)
          case BOp.Div => Option.when(y != 0)(x / y)
          case BOp.Pow =>
            Option.when(y.isValidInt && y >= 0 && y <= 1024)(x.pow(y.toInt))
          case _ => None
      } yield z
    case _ => None

  private def observeBatch(
    exprs: List[String],
  ): Map[String, Try[Option[ValueTy]]] =
    if (exprs.isEmpty) Map()
    else
      try {
        val src = ("var __a__ = 0;" :: exprs.zipWithIndex.map { (e, i) =>
          s"var __w${i}__; try { __w${i}__ = ($e); } catch (e) {}"
        }).mkString("\n")
        val st = Interpreter(cfg.init.from(src), timeLimit = Some(20))
        val globals = st.heap.map.collectFirst {
          case (_, m: MapObj) if m.map.contains(Str("__a__")) => m
        }
        exprs.zipWithIndex.map { (expr, i) =>
          val ty = for {
            g <- globals
            addr <- g.map.get(Str(s"__w${i}__")).collect { case a: Addr => a }
            obj <- st.heap.map.get(addr).collect { case r: RecordObj => r }
            value <- obj.map.get("Value")
          } yield st.typeOf(value)
          expr -> Success(ty)
        }.toMap
      } catch {
        case _: Throwable if exprs.size > 1 =>
          val (l, r) = exprs.splitAt(exprs.size / 2)
          observeBatch(l) ++ observeBatch(r)
        case cause: Throwable => Map(exprs.head -> Failure(cause))
      }

  lazy val manualEntries: List[Manual] =
    def started(generator: String): String =
      s"(() => { const g = ($generator)(); g.next(); return g; })()"
    val revoked = RecordT(
      "ProxyExoticObject",
      Map("ProxyTarget" -> NullT, "ProxyHandler" -> NullT),
    )
    val detached = RecordT("ArrayBuffer", Map("ArrayBufferData" -> NullT))
    val rows: List[(ValueTy, List[String])] = List(
      // NOTE: basic seeds (17)
      ObjectT -> List("{}"),
      SymbolT -> List("Symbol()"),
      FunctionT -> List("() => {}"),
      RecordT("ECMAScriptFunctionObject") -> List("function(){}"),
      RecordT("BoundFunctionExoticObject") -> List("(function(){}).bind()"),
      RecordT("BuiltinFunctionObject", List("Call", "Construct")) -> List(
        "Object",
      ),
      RecordT("BuiltinFunctionObject", List("Call")) -> List("Math.max"),
      RecordT("ArgumentsExoticObject") -> List(
        "(function(){ return arguments; })()",
      ),
      RecordT("ErrorObject") -> List("new Error()"),
      RecordT("SettledPromise") -> List("Promise.resolve(0)"),
      RecordT("PendingPromise") -> List("new Promise(() => {})"),
      RecordT("Generator") -> List("(function*(){})()"),
      RecordT(
        "Generator",
        Map("GeneratorState" -> EnumT("completed")),
      ) -> List(started("function*(){}")),
      RecordT(
        "Generator",
        Map("GeneratorState" -> EnumT("suspended-yield")),
      ) -> List(started("function*(){ yield 0; }")),
      RecordT("AsyncGenerator") -> List("(async function*(){})()"),
      RecordT(
        "AsyncGenerator",
        Map("AsyncGeneratorState" -> EnumT("completed")),
      ) -> List(started("async function*(){}")),
      RecordT(
        "AsyncGenerator",
        Map("AsyncGeneratorState" -> EnumT("draining-queue")),
      ) -> List(started("async function*(){ yield 0; }")),
      // FIXME: derivation edge-cases (2)
      RecordT("StringExoticObject") -> List("Object('')"),
      RecordT("ArrayBuffer", Map("ArrayBufferMaxByteLength" -> AnyT)) -> List(
        "new ArrayBuffer(0, { maxByteLength: 0 })",
      ),
      // FIXME: size/content seeds (33)
      StrT -> List("\"\"", "\"aa\""),
      ArrayT -> List("[]", "[0]", "[0, 0]"),
      RecordT("Map") -> List("new Map([])", "new Map([[0, 0]])"),
      RecordT("Set") -> List("new Set([])", "new Set([0])"),
      RecordT("WeakMap") -> List("new WeakMap([[{}, 0]])"),
      RecordT("WeakSet") -> List("new WeakSet([{}])"),
      RecordT("ArrayBuffer") -> List(
        "new ArrayBuffer(0)",
        "new ArrayBuffer(8)",
      ),
      TypedArrayT -> List(
        "new Int8Array([])",
        "new Int8Array([0])",
        "new Int8Array([1])",
        "new Int8Array([0, 0])",
      ),
      RecordT("Int8Array") -> List(
        "new Int8Array([])",
        "new Int8Array([0])",
        "new Int8Array([1])",
        "new Int8Array([0, 0])",
      ),
      RecordT("Uint8Array") -> List("new Uint8Array([0])"),
      RecordT("Uint8ClampedArray") -> List("new Uint8ClampedArray([])"),
      RecordT("Int16Array") -> List("new Int16Array([])"),
      RecordT("Uint16Array") -> List("new Uint16Array([])"),
      RecordT("Int32Array") -> List("new Int32Array([])"),
      RecordT("Uint32Array") -> List("new Uint32Array([])"),
      RecordT("BigInt64Array") -> List(
        "new BigInt64Array([])",
        "new BigInt64Array([0n])",
      ),
      RecordT("BigUint64Array") -> List("new BigUint64Array([])"),
      RecordT("Float16Array") -> List("new Float16Array([])"),
      RecordT("Float32Array") -> List("new Float32Array([])"),
      RecordT("Float64Array") -> List("new Float64Array([])"),
      // FIXME: using revoked proxy (1)
      revoked -> List(
        "(() => { const r = Proxy.revocable(function(){}, {}); " +
        "r.revoke(); return r.proxy; })()",
      ),
      // FIXME: using detached arraybuffer (13)
      detached -> List(
        "(() => { const b = new ArrayBuffer(8); b.transfer(); return b; })()",
      ),
      RecordT("Int8Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Int8Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Uint8Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Uint8Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT(
        "Uint8ClampedArray",
        Map("ViewedArrayBuffer" -> detached),
      ) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Uint8ClampedArray(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Int16Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Int16Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Uint16Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Uint16Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Int32Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Int32Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Uint32Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Uint32Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("BigInt64Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new BigInt64Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT(
        "BigUint64Array",
        Map("ViewedArrayBuffer" -> detached),
      ) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new BigUint64Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Float16Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Float16Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Float32Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Float32Array(b); " +
        "b.transfer(); return t; })()",
      ),
      RecordT("Float64Array", Map("ViewedArrayBuffer" -> detached)) -> List(
        "(() => { const b = new ArrayBuffer(8); const t = new Float64Array(b); " +
        "b.transfer(); return t; })()",
      ),
    )
    val observations = rows
      .flatMap(_._2)
      .distinct
      .grouped(120)
      .flatMap(observeBatch)
      .toMap
    rows.flatMap { (ty, exprs) =>
      exprs.map { expr =>
        Manual(ty, expr, observations(expr))
      }
    }
}

object TySynthesizer {
  private case class Literals(
    numbers: List[Number],
    bigInts: List[SBigInt],
    strings: List[String],
  )

  case class Manual(
    annotation: ValueTy,
    expr: String,
    observation: Try[Option[ValueTy]],
  ) {
    val ty: ValueTy = observation.toOption.flatten
      .filter(ty => !ty.isBottom && !ty.record.isBottom && ty <= annotation)
      .getOrElse(annotation)
  }
}
