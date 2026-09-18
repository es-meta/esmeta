package esmeta.solver

import esmeta.cfg.CFG
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.solver.Solver.Invocation
import esmeta.solver.TemplateGenerator.{Template, getSlots}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

class TySynthesizer(
  cfg: CFG,
  templatesBySlot: Map[String, List[Template]],
) {
  import TySynthesizer.*

  /** sample a candidate expression for a required type */
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
          () => fromShape(valueTy),
          () => fromTemplate(valueTy),
        ),
      )
  }

  /** synthesize an expression for each hole */
  def synthesize(holes: List[(String, ValueTy)])(using
    checkDeadline: () => Unit,
  ): Option[Map[String, String]] = {
    checkDeadline()
    holes.foldLeft(Option(Map.empty[String, String])) {
      case (values, (hole, ty)) =>
        values.flatMap { vs =>
          checkDeadline()
          synthesize(ty).map(value => vs.updated(hole, value))
        }
    }
  }

  private def firstSuccess[A](choices: List[() => Option[A]]): Option[A] =
    shuffle(choices).iterator.flatMap(_()).nextOption()

  private val cachedManuals = TrieMap[ValueTy, List[String]]()

  private def fromDirect(ty: ValueTy): Option[String] = {
    val values = (primitives(ty) ++ manualExprs(ty)).distinct
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
  private def fromShape(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record match {
    case RecordTy.Elem(map, ObjShape(props, call, construct))
        if props.nonEmpty =>
      val ordered = props.toList.sortBy { case (prop, _) => propKey(prop) }
      def objectLiteral(): Option[(String, List[(Property, String)])] =
        ordered
          .foldLeft(Option(List.empty[(Property, String, String)])) {
            case (fields, (prop, desc)) =>
              fields.flatMap { fs =>
                val key = propKey(prop)
                val field = firstSuccess(
                  List(
                    () =>
                      Option.when(desc.getExc)(
                        "get" -> s"get $key() { throw 0; }",
                      ),
                    () =>
                      Option.when(desc.setExc)(
                        "set" -> s"set $key(_) { throw 0; }",
                      ),
                    () => synthesize(desc.ty).map(v => "value" -> s"$key: $v"),
                  ),
                )
                field.map((kind, code) => (prop, kind, code) :: fs)
              }
          }
          .map { fields =>
            val orderedFields = fields.reverse
            orderedFields.map(_._3).mkString("{ ", ", ", " }") ->
            orderedFields.map((prop, kind, _) => prop -> kind)
          }
      if (isPlainObject(ty) && !call.exists && !construct.exists)
        objectLiteral().map(_._1)
      else {
        val baseTy = ty.copied(record =
          RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
        )
        val overlay = () => {
          for {
            base <- synthesize(baseTy)
            (obj, fields) <- objectLiteral()
          } yield {
            // preserve existing attributes and unspecified accessors
            val updates = fields
              .map { (prop, field) =>
                val key = propExpr(prop)
                s"[$key]: Object.getOwnPropertyDescriptor(o, $key) ? " +
                s"{ $field: ds[$key].$field } : ds[$key]"
              }
              .mkString("{ ", ", ", " }")
            s"((o, ds) => Object.defineProperties(o, $updates))" +
            s"($base, Object.getOwnPropertyDescriptors($obj))"
          }
        }
        val proxy = ordered match {
          case (prop, desc) :: Nil =>
            List(() => {
              val key = propExpr(prop)
              for {
                base <- synthesize(baseTy)
                handler <- firstSuccess(
                  List(
                    () =>
                      Option.when(desc.getExc)(
                        s"get(t, p, r) { if (p === $key) throw 0; return Reflect.get(t, p, r); }",
                      ),
                    () =>
                      Option.when(desc.setExc)(
                        s"set(t, p, v, r) { if (p === $key) throw 0; return Reflect.set(t, p, v, r); }",
                      ),
                    () =>
                      synthesize(desc.ty).map(v =>
                        s"get(t, p, r) { if (p === $key) return $v; return Reflect.get(t, p, r); }",
                      ),
                  ),
                )
              } yield s"new Proxy($base, { $handler })"
            })
          case _ => Nil
        }
        firstSuccess(overlay :: proxy)
      }
    case _ =>
      firstSuccess(
        List(
          () => fromConstruct(ty),
          () => fromCall(ty),
        ),
      )
  }

  private def fromConstruct(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record.construct match {
    case ConstructDesc.Elem(exc, ret) =>
      firstSuccess(
        List(
          () => Option.when(exc)("function() { throw 0; }"),
          () => synthesize(ret).map(v => s"function() { return $v; }"),
        ),
      )
    case ConstructDesc.Top => None
  }

  private def fromCall(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] = ty.record.call match {
    case CallDesc.Elem(exc, ret) =>
      val isCtor = ty <= ConstructorT
      firstSuccess(
        List(
          () =>
            Option.when(exc)(
              if (isCtor) "function() { throw 0; }"
              else "() => { throw 0; }",
            ),
          () =>
            synthesize(ret).map { value =>
              if (isCtor) s"function() { return $value; }"
              else s"() => ($value)"
            },
        ),
      )
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

  private def manualExprs(ty: ValueTy): List[String] =
    if (ty.isBottom) Nil
    else
      cachedManuals.getOrElseUpdate(
        ty,
        observations.filter(obs => matches(ty, obs.value, obs.heap)).map(_.expr),
      )

  // check refined slots even when the record has a subtype tag
  private def matches(ty: ValueTy, value: Value, heap: Heap): Boolean =
    ty.safeContains(value, heap).contains(true) && ((value, ty.record) match {
      case (addr: Addr, RecordTy.Elem(map, _)) =>
        heap(addr) match {
          case record: RecordObj =>
            map.exists { (name, fields) =>
              RecordT(name).safeContains(value, heap).contains(true) &&
              fields.map.forall { (field, binding) =>
                record.get(field).fold(binding.absent) { value =>
                  matches(binding.value, value, heap)
                }
              }
            }
          case _ => true
        }
      case _ => true
    })

  private def fromTemplate(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): Option[String] =
    shuffle(matchingTemplates(ty)).iterator
      .flatMap(instantiate)
      .nextOption()

  /** instantiate constrained holes with recursively synthesized expressions */
  private def instantiate(invocation: Invocation)(using
    checkDeadline: () => Unit,
  ): Option[String] = {
    checkDeadline()
    invocation.form
      .filter { (_, holes) =>
        holes.forall { (_, input) =>
          checkDeadline()
          !(input && ESValueT).isBottom
        }
      }
      .flatMap { (expr, holes) =>
        synthesize(holes).map(values => Invocation.fill(expr, values))
      }
  }

  private def matchingTemplates(ty: ValueTy)(using
    checkDeadline: () => Unit,
  ): List[Invocation] =
    getSlots(ty).toList.sorted
      .flatMap(field => templatesBySlot.getOrElse(field, Nil))
      .distinct
      .flatMap { template =>
        checkDeadline()
        val upper = template.returnTy
        if (upper ⊑ ty) List(template.invocation)
        else if (upper overlaps ty) template.specialize(ty).toList
        else Nil
      }
      .distinct

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

  private val observations: List[ObservedExpr] =
    manuals.grouped(120).flatMap(observeBatch).toList

  private def observeBatch(exprs: List[String]): List[ObservedExpr] =
    if (exprs.isEmpty) Nil
    else
      try {
        val src =
          ("var __marker__ = 0;" :: exprs.zipWithIndex.map { (expr, i) =>
            s"var __value${i}__, __succeeded${i}__ = false; " +
            s"try { __value${i}__ = ($expr); " +
            s"__succeeded${i}__ = true; } catch (e) {}"
          }).mkString("\n")
        val st = Interpreter(cfg.init.from(src), timeLimit = Some(20))
        val globals = st.heap.map.collectFirst {
          case (_, m: MapObj) if m.map.contains(Str("__marker__")) => m
        }
        (for {
          (expr, i) <- exprs.zipWithIndex
          properties <- globals
          succeededDesc <- properties.map.get(Str(s"__succeeded${i}__"))
          if st(succeededDesc, Str("Value")) == Bool(true)
          valueDesc <- properties.map.get(Str(s"__value${i}__"))
        } yield ObservedExpr(expr, st(valueDesc, Str("Value")), st.heap)).toList
      } catch {
        case _: Throwable if exprs.size > 1 =>
          val (l, r) = exprs.splitAt(exprs.size / 2)
          observeBatch(l) ++ observeBatch(r)
        case _: Throwable => Nil
      }

  lazy val manuals: List[String] =
    // basic values and syntax (7)
    val ordinaryObjects = List("{}")
    val symbols = List("Symbol()")
    val argumentsObjects = List("(function(){ return arguments; })()")
    val ecmascriptFunctions = List("() => {}", "function(){}")
    val freshGenerators = List("(function*(){})()", "(async function*(){})()")

    // size variations (6)
    val strings = List("\"\"", "\"a\"", "\"aa\"")
    val arrays = List("[]", "[0]", "[0, 0]")

    // builtin references and results (4)
    val builtinFunctions = List(
      "Object", // callable and constructable
      "Math.max", // callable only
    )
    val boundFunctions = List("(function(){}).bind()")
    val errors = List("new Error()")

    // execution states (20)
    def afterNext(generator: String): String =
      s"(() => { const g = ($generator)(); g.next(); return g; })()"

    def revoked(target: String): String =
      s"(() => { const r = Proxy.revocable($target, {}); " +
      "r.revoke(); return r.proxy; })()"

    def withDetachedBuffer(makeValue: String => String): String =
      "(() => { const buffer = new ArrayBuffer(8); " +
      s"const value = ${makeValue("buffer")}; " +
      "buffer.transfer(); return value; })()"

    val promises = List(
      "Promise.resolve(0)",
      "new Promise(() => {})",
    )
    val resumedGenerators = List(
      afterNext("function*(){}"),
      afterNext("function*(){ yield 0; }"),
      afterNext("async function*(){}"),
      afterNext("async function*(){ yield 0; }"),
    )
    val revokedProxies = List(revoked("function(){}"))
    val detachedBuffers = List(withDetachedBuffer(buffer => buffer))
    val detachedTypedArrays = cfg.init.taNames.map { name =>
      withDetachedBuffer(buffer => s"new $name($buffer)")
    }

    val expressions =
      ordinaryObjects ++ symbols ++ argumentsObjects ++ ecmascriptFunctions ++
      freshGenerators ++ strings ++ arrays ++
      builtinFunctions ++ boundFunctions ++ errors ++
      promises ++ resumedGenerators ++ revokedProxies ++ detachedBuffers ++
      detachedTypedArrays
    expressions.distinct

}

object TySynthesizer {
  case class Literals(
    numbers: List[Number],
    bigInts: List[SBigInt],
    strings: List[String],
  )

  // keep the heap for internal-slot checks on object values
  case class ObservedExpr(expr: String, value: Value, heap: Heap)
}
