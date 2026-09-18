package esmeta.solver

import esmeta.cfg.CFG
import esmeta.interpreter.Interpreter
import esmeta.solver.Solver.Invocation
import esmeta.solver.TemplateGenerator.{Template, getSlots}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.concurrent.TrieMap

class ExprSynthesizer(
  cfg: CFG,
  templatesBySlot: Map[String, List[Template]],
) {
  import ExprSynthesizer.*

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

  private val cachedDirect = TrieMap[ValueTy, List[List[String]]]()

  // type literals and matching candidates, sampled by kind and then by value
  private def fromDirect(ty: ValueTy): Option[String] = {
    val groups = cachedDirect.getOrElseUpdate(
      ty, {
        val numbers = ty.number.toNumberSet.toList
          .flatMap(_.toList.sortBy(n => (n.isNaN, n.double)))
          .map(n => "Number" -> numberLit(n))
        val strings = ty.str match
          case Fin(set) =>
            set.toList.sorted.map(s => "Str" -> s"\"${normStr(s)}\"")
          case Inf => Nil
        val observed = observations
          .filter(obs => matches(ty, obs.value, obs.heap))
          .map(obs => kindOf(obs.value, obs.heap) -> obs.expr)
        (numbers ++ strings ++ observed)
          .groupMap(_._1)(_._2)
          .toList
          .sortBy(_._1)
          .map(_._2.distinct)
      },
    )
    Option.when(groups.nonEmpty)(choose(choose(groups)))
  }

  // sampling stratum of an observed value
  private def kindOf(value: Value, heap: Heap): String = value match
    case addr: Addr =>
      heap(addr) match
        case record: RecordObj => record.tname
        case _: MapObj         => "Map"
        case _: ListObj        => "List"
        case _                 => "Object"
    case _: Number => "Number"
    case _: BigInt => "BigInt"
    case _: Str    => "Str"
    case _: Bool   => "Bool"
    case Undef     => "Undefined"
    case Null      => "Null"
    case _         => "Other"

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

  private val observations: List[ObservedExpr] = observeBatch(manuals)

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
    // primitive values (26)
    val nullish = List("undefined", "null")
    val booleans = List("true", "false")
    val strings = List("\"\"", "\"a\"", "\"aa\"")
    val symbols = List("Symbol()")
    val nonFinite = List("NaN", "-Infinity", "Infinity")
    val zeros = List("-0", "0")
    val fractions = List("-0.5", "0.5")
    val integers = List("-1", "1", "2", "4", "8")
    val limits = List(
      "Number.MIN_VALUE",
      "Number.MAX_SAFE_INTEGER",
      "Number.MAX_VALUE",
    )
    val bigInts = List("-1n", "0n", "1n")

    // object values (16)
    val ordinaryObjects = List("{}")
    val arrays = List("[]", "[0]", "[0, 0]")
    val argumentsObjects = List("(function(){ return arguments; })()")
    val ecmascriptFunctions = List("() => {}", "function(){}")
    val classConstructors = List("class {}", "class extends Object {}")
    val builtinFunctions = List(
      "Object", // callable and constructable
      "Function.prototype", // callable only
    )
    val boundFunctions = List("(function(){}).bind()")
    val errors = List("new Error()")
    val promises = List("new Promise(() => {})")
    val generators = List("(function*(){})()", "(async function*(){})()")

    // execution states (22)
    def afterThen(promise: String): String =
      s"(() => { const p = $promise; p.then(); return p; })()"

    def afterNext(generator: String): String =
      s"(() => { const g = ($generator)(); g.next(); return g; })()"

    def revoked(target: String): String = oneLine(
      s"""(() => {
         |  const r = Proxy.revocable($target, {});
         |  r.revoke();
         |  return r.proxy;
         |})()""",
    )

    def withDetachedBuffer(makeValue: String => String): String = oneLine(
      s"""(() => {
         |  const buffer = new ArrayBuffer(8);
         |  const value = ${makeValue("buffer")};
         |  buffer.transfer();
         |  return value;
         |})()""",
    )

    val settledPromises = List(
      "Promise.resolve(0)",
      "Promise.reject(0)",
      afterThen("Promise.reject(0)"),
    )
    val resumedGenerators = List(
      afterNext("function*(){}"),
      afterNext("function*(){ yield 0; }"),
      afterNext("async function*(){}"),
      afterNext("async function*(){ yield 0; }"),
    )
    val revokedProxies = List(revoked("function(){}"))
    val resizableBuffers = List("new ArrayBuffer(8, { maxByteLength: 16 })")
    val detachedBuffers = List(withDetachedBuffer(buffer => buffer))
    val detachedTypedArrays = cfg.init.taNames.map { name =>
      withDetachedBuffer(buffer => s"new $name($buffer)")
    }

    List(
      nullish,
      booleans,
      strings,
      symbols,
      nonFinite,
      zeros,
      fractions,
      integers,
      limits,
      bigInts,
      ordinaryObjects,
      arrays,
      argumentsObjects,
      ecmascriptFunctions,
      classConstructors,
      builtinFunctions,
      boundFunctions,
      errors,
      promises,
      generators,
      settledPromises,
      resumedGenerators,
      revokedProxies,
      resizableBuffers,
      detachedBuffers,
      detachedTypedArrays,
    ).flatten.distinct
}

object ExprSynthesizer {
  // keep the heap for internal-slot checks on object values
  case class ObservedExpr(expr: String, value: Value, heap: Heap)
}
