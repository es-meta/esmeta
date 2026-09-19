package esmeta.solver

import esmeta.cfg.CFG
import esmeta.es.builtin.INNER_MAP
import esmeta.interpreter.Interpreter
import esmeta.ir.Expr
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
          .map(n => numberLit(n) -> NumberT(n))
        val strings = ty.str match
          case Fin(set) =>
            set.toList.sorted.map(s => s"\"${normStr(s)}\"" -> StrT(s))
          case Inf => Nil
        val records = ty.copied(record = ty.record match
          case RecordTy.Elem(map, _) => RecordTy.Elem(map)
          case other                 => other,
        )
        val observed = observations.toList.filter(_._2 <= records)
        (numbers ++ strings ++ observed)
          .groupMap((_, valueTy) => kindOf(valueTy))(_._1)
          .toList
          .sortBy(_._1)
          .map(_._2.distinct)
      },
    )
    Option.when(groups.nonEmpty)(choose(choose(groups)))
  }

  // sampling stratum: the ECMAScript language type
  private def kindOf(ty: ValueTy): String =
    if (ty <= UndefT) "Undefined"
    else if (ty <= NullT) "Null"
    else if (ty <= BoolT) "Boolean"
    else if (ty <= StrT) "String"
    else if (ty <= SymbolT) "Symbol"
    else if (ty <= NumberT) "Number"
    else if (ty <= BigIntT) "BigInt"
    else "Object"

  // type of an observed value
  private def observedTy(value: Value, st: State, depth: Int): ValueTy =
    value match
      case addr: Addr if depth > 0 =>
        st.heap(addr) match
          case RecordObj(tname, map) =>
            val absent = ManualInfo.tyModel.fieldsOf(tname).collect {
              case (f, binding) if binding.absent && !map.contains(f) =>
                f -> Binding.Absent
            }
            val fields = map.map { (f, v) =>
              f -> Binding(observedTy(v, st, depth - 1))
            }
            RecordT(tname, FieldMap(fields.toMap ++ absent))
          case obj => st.typeOf(obj, detail = false)
      case n: Number => NumberT(n)
      case Str(s)    => StrT(s)
      case Bool(b)   => BoolT(b)
      case _         => st.typeOf(value, detail = false)

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
      def members(): Option[List[(Property, String, String)]] =
        ordered
          .foldLeft(Option(List.empty[(Property, String, String)])) {
            case (members, (prop, desc)) =>
              members.flatMap { ms =>
                val member = firstSuccess(
                  List(
                    () => Option.when(desc.getExc)("get" -> ""),
                    () => Option.when(desc.setExc)("set" -> ""),
                    () => synthesize(desc.ty).map("value" -> _),
                  ),
                )
                member.map((kind, value) => (prop, kind, value) :: ms)
              }
          }
          .map(_.reverse)
      def literal(ms: List[(Property, String, String)]): String =
        ms.map { (prop, kind, value) =>
          val key = propKey(prop)
          kind match
            case "get" => s"get $key() { throw 0; }"
            case "set" => s"set $key(_) { throw 0; }"
            case _     => s"$key: $value"
        }.mkString("{ ", ", ", " }")
      def descriptors(ms: List[(Property, String, String)]): String =
        ms.map { (prop, kind, value) =>
          val key = propKey(prop)
          kind match
            case "get" => s"$key: { get() { throw 0; } }"
            case "set" => s"$key: { set(_) { throw 0; } }"
            case _     => s"$key: { value: $value }"
        }.mkString("{ ", ", ", " }")
      if (isPlainObject(ty) && !call.exists && !construct.exists)
        members().map(literal)
      else {
        val baseTy = ty.copied(record =
          RecordTy.Elem(map, ObjShape(Map.empty, call, construct)),
        )
        val overlay = () => {
          for {
            base <- synthesize(baseTy)
            ms <- members()
          } yield s"Object.defineProperties($base, ${descriptors(ms)})"
        }
        val proxy = ordered match {
          case (prop, desc) :: Nil if (desc.getExc || desc.setExc) =>
            List(() => {
              val key = propExpr(prop)
              for {
                base <- synthesize(baseTy)
                handler <- firstSuccess(
                  List(
                    () =>
                      Option.when(desc.getExc)(
                        oneLine(
                          s"""get(t, p, r) {
                           |  if (p === $key) throw 0;
                           |  return Reflect.get(t, p, r);
                           |}""",
                        ),
                      ),
                    () =>
                      Option.when(desc.setExc)(
                        oneLine(
                          s"""set(t, p, v, r) {
                           |  if (p === $key) throw 0;
                           |  return Reflect.set(t, p, v, r);
                           |}""",
                        ),
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
              else {
                if (value.startsWith("{")) s"() => ($value)"
                else s"() => $value"
              }
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

  private val identifier = "[A-Za-z_$][\\w$]*".r

  private def propKey(prop: Property): String = prop match
    case Property.PStr("__proto__")                      => "[\"__proto__\"]"
    case Property.PStr(str) if (identifier.matches(str)) => str
    case Property.PStr(str) => s"\"${normStr(str)}\""
    case Property.PSym(sym) => s"[Symbol.$sym]"

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

  // evaluate the manual expressions once to match them against types
  private val observations: Map[String, ValueTy] = {
    val src = manuals.zipWithIndex
      .map { (expr, i) =>
        s"""var __value${i}__, __succeeded${i}__ = false;
           |try {
           |  __value${i}__ = ($expr);
           |  __succeeded${i}__ = true;
           |} catch (e) {}""".stripMargin
      }
      .mkString("\n")
    val st = Interpreter(cfg.init.from(src), timeLimit = Some(20))
    val reader = new Interpreter(st.copied)
    def global(name: String): Value =
      val path = s"""@REALM.GlobalObject.$INNER_MAP["$name"].Value"""
      reader.eval(Expr.from(path))
    (for {
      (expr, i) <- manuals.zipWithIndex
      if global(s"__succeeded${i}__") == Bool(true)
      value = global(s"__value${i}__")
    } yield expr -> observedTy(value, st, RecordTy.maxFieldDepth)).toMap
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
