package esmeta.opcount

import esmeta.extractor.Extractor
import esmeta.lang.*
import esmeta.lang.util.UnitWalker
import esmeta.spec.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{Map => MMap, Set => MSet, ListBuffer}

/** Census of atomic operations over *every* built-in of the current spec
  * (the ecma262 submodule, ES2025), not only those DynaJS extracts.
  *
  * For each built-in function (`BuiltinHead`), follow invoked algorithms
  * (abstract operations, numeric methods, syntax-directed operations other
  * than `Evaluation`) and stop at internal/concrete methods (the object
  * operations `get`/`set`/`call` sit there) and at host-/implementation-defined
  * abstract operations, which are recorded as boundary calls.
  *
  * output: {
  *   builtins: {built-in: [algorithm, ...]},
  *   algorithms: {algorithm: {kind, ops, defaults, yets, blocked, kinds,
  *                            boundary, unresolved}}
  * }
  *
  * usage: AllBuiltins <out.json>
  */
object AllBuiltins {
  def main(args: Array[String]): Unit =
    val out = args(0)
    val spec = Extractor()
    val algs = spec.algorithms
    val byName: Map[String, Algorithm] = algs.map(a => a.name -> a).toMap
    val sdoByMethod: Map[String, List[Algorithm]] = algs.collect {
      case a @ Algorithm(h: SyntaxDirectedOperationHead, _, _) =>
        h.methodName -> a
    }.groupMap(_._1)(_._2)

    def headKind(a: Algorithm): String = a.head match
      case h: AbstractOperationHead =>
        if (h.isHostDefined) "host-defined" else "abstract operation"
      case _: NumericMethodHead           => "numeric method"
      case _: SyntaxDirectedOperationHead => "sdo"
      case _: ConcreteMethodHead          => "concrete method"
      case _: InternalMethodHead          => "internal method"
      case _: BuiltinHead                 => "built-in"
      case _                              => a.head.getClass.getSimpleName

    // per-algorithm analysis, memoized
    case class Info(
      kind: String,
      code: String,
      ops: Map[String, Int],
      defaults: Map[String, Int],
      yets: List[String],
      blocked: List[String],
      kinds: Map[String, Int],
      callees: List[String],
      conv: Map[String, Int],
      boundary: List[String],
      unresolved: List[String],
    )
    val infos = MMap[String, Info]()

    def analyze(a: Algorithm): Info = infos.getOrElseUpdate(
      a.name, {
        val c = ParseAlgs.Counter()
        val oc = OpCounter()
        val kd = ParseAlgs.Kinds()
        val bl = ParseAlgs.Blocked()
        val dc = DefaultCounter()
        val callees = MSet[String]()
        val boundary = MSet[String]()
        val unresolved = MSet[String]()
        def target(name: String): Unit = byName.get(name) match
          case Some(t) =>
            t.head match
              case h: AbstractOperationHead if h.isHostDefined =>
                boundary += name
              case _ =>
                if (Option(t.elem.closest("emu-clause")).exists(_.attr("type").startsWith("implementation-defined")))
                  boundary += name
                else callees += name
          case None => unresolved += name
        val conv = MMap[String, Int]().withDefaultValue(0)
        val calls = new UnitWalker {
          override def walk(expr: CalcExpression): Unit =
            expr match
              case ConversionExpression(op, _, _) => conv(op.toString) += 1
              case _ =>
            super.walk(expr)
          override def walk(expr: Expression): Unit =
            expr match
              case MathOpExpression(op, _) => conv(s"mathop:$op") += 1
              case InvokeAbstractOperationExpression(name, _, _) =>
                target(name)
              case InvokeNumericMethodExpression(base, name, _) =>
                target(s"$base::$name")
              case InvokeMethodExpression(access, _, _) =>
                boundary += s"method:${access.name}"
              case InvokeSyntaxDirectedOperationExpression(_, "Evaluation", _, _, _) =>
                boundary += "sdo:Evaluation"
              case InvokeSyntaxDirectedOperationExpression(_, name, _, _, _) =>
                sdoByMethod.get(name) match
                  case Some(ts) => callees ++= ts.map(_.name)
                  case None     => unresolved += s"sdo:$name"
              case _ =>
            super.walk(expr)
        }
        val body = a.body
        c.walk(body); oc.count(body); kd.walk(body); bl.walk(body)
        dc.walk(body); calls.walk(body)
        Info(
          headKind(a),
          a.code,
          oc.ops.toMap,
          dc.sites.toMap,
          c.yets.toList,
          bl.blocked.toList,
          kd.kinds.toMap,
          callees.toList.sorted,
          conv.toMap,
          boundary.toList.sorted,
          unresolved.toList.sorted,
        )
      },
    )

    val builtins = algs.filter(_.head.isInstanceOf[BuiltinHead])
    val closures = for (b <- builtins) yield {
      val seen = MSet[String]()
      val order = ListBuffer[String]()
      val todo = scala.collection.mutable.Queue(b.name)
      while (todo.nonEmpty) {
        val n = todo.dequeue()
        if (!seen(n)) {
          seen += n; order += n
          val info = analyze(byName(n))
          todo ++= info.callees.filterNot(seen)
        }
      }
      b.name -> order.toList
    }

    given Encoder[Info] = Encoder.instance { i =>
      Json.obj(
        "kind" -> i.kind.asJson,
        "code" -> i.code.asJson,
        "ops" -> i.ops.asJson,
        "defaults" -> i.defaults.asJson,
        "yets" -> i.yets.asJson,
        "blocked" -> i.blocked.asJson,
        "kinds" -> i.kinds.asJson,
        "callees" -> i.callees.asJson,
        "conv" -> i.conv.asJson,
        "boundary" -> i.boundary.asJson,
        "unresolved" -> i.unresolved.asJson,
      )
    }
    val json = Json.obj(
      "version" -> spec.version.map(_.toString).getOrElse("?").asJson,
      "builtins" -> closures.toMap.asJson,
      "algorithms" -> infos.toMap.asJson,
    )
    dumpFile(json.spaces2, out)
    println(s"${builtins.length} built-ins, ${infos.size} algorithms -> $out")
}
