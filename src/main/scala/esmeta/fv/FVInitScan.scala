package esmeta.fv

import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.*
import esmeta.extractor.Extractor
import esmeta.state.*

/** Measure the initial state a Test262 run starts from, to decide whether
  * exporting it into Rocq is feasible at all (stage G4 of the Test262
  * goal explicitly says to stop and ask if it is too large to compile).
  *
  * Reports object counts by kind, total field count, and the shape of the
  * cached AST for a trivial script. Measurement only — writes nothing.
  *
  * Usage: sbt "runMain esmeta.fv.FVInitScan [source]"
  */
object FVInitScan {

  def astStats(ast: Ast): (Int, Int, Int) = ast match
    case lex: Lexical => (1, 0, 1)
    case syn: Syntactic =>
      syn.children.flatten.foldLeft((1, 1, 0)) {
        case ((n, s, l), c) =>
          val (cn, cs, cl) = astStats(c)
          (n + cn, s + cs, l + cl)
      }

  def main(args: Array[String]): Unit = {
    val source = if (args.nonEmpty) args(0) else "var x = 1;"
    println("[fv] extracting spec and building CFG (this takes a while)")
    val cfg = CFGBuilder(Compiler(Extractor()))
    println(s"[fv] spec functions in CFG: ${cfg.funcs.size}")

    val st = Initialize(cfg).from(source)

    // globals
    println(s"[fv] initial globals: ${st.globals.size}")

    // heap
    val objs = st.heap.map.values.toList
    var records, lists, maps, others = 0
    var fields = 0
    for (o <- objs) o match
      case r: RecordObj => records += 1; fields += r.map.size
      case l: ListObj   => lists += 1; fields += l.values.size
      case m: MapObj    => maps += 1; fields += m.map.size
      case _            => others += 1
    println(s"[fv] initial heap objects: ${objs.size}")
    println(s"[fv]   records=$records lists=$lists maps=$maps other=$others")
    println(s"[fv]   total fields/elements: $fields")

    // how big is the whole spec IR as a Rocq term?
    var okFuncs, badFuncs, chars = 0
    val reasons = scala.collection.mutable.Map[String, Int]()
    for (f <- cfg.program.funcs) {
      try { chars += FVExport.rocqFunc(f).length; okFuncs += 1 }
      catch {
        case FVExport.Unsupported(msg) =>
          badFuncs += 1
          val key =
            if (msg.startsWith("ty: ")) msg else msg.takeWhile(_ != ':')
          reasons(key) = reasons.getOrElse(key, 0) + 1
      }
    }
    println(s"[fv] spec funcs translatable: $okFuncs, rejected: $badFuncs")
    println(f"[fv] Rocq term size for the translatable ones: " +
      f"${chars / 1024.0 / 1024.0}%.2f MiB")
    for ((k, n) <- reasons.toList.sortBy(-_._2).take(25)) println(f"[fv]   $n%5d  $k")
    println(s"[fv] distinct rejection reasons: ${reasons.size}")

    // census of every type test in the spec (not just first-blocker)
    val tyHist = scala.collection.mutable.Map[String, Int]()
    val w = new esmeta.ir.util.UnitWalker {
      override def walk(e: esmeta.ir.Expr): Unit = {
        e match
          case esmeta.ir.ETypeCheck(_, t) =>
            val k = t.ty.toString
            tyHist(k) = tyHist.getOrElse(k, 0) + 1
          case _ => ()
        super.walk(e)
      }
    }
    for (f <- cfg.program.funcs) w.walk(f.body)
    val tot = tyHist.values.sum
    println(s"[fv] ETypeCheck occurrences: $tot over ${tyHist.size} distinct types")
    for ((k, n) <- tyHist.toList.sortBy(-_._2).take(20))
      println(f"[fv]   $n%5d  $k")

    // which record type names does the spec actually ALLOCATE?
    val recHist = scala.collection.mutable.Map[String, Int]()
    val w2 = new esmeta.ir.util.UnitWalker {
      override def walk(e: esmeta.ir.Expr): Unit = {
        e match
          case esmeta.ir.ERecord(tname, _) =>
            recHist(tname) = recHist.getOrElse(tname, 0) + 1
          case _ => ()
        super.walk(e)
      }
    }
    for (f <- cfg.program.funcs) w2.walk(f.body)
    val comp = recHist.toList.filter(_._1.contains("Completion")).sortBy(-_._2)
    println(s"[fv] ERecord allocations of Completion-ish types:")
    for ((k, n) <- comp) println(f"[fv]   $n%5d  $k")
    // and in the initial heap
    val heapNames = scala.collection.mutable.Map[String, Int]()
    for (o <- st.heap.map.values) o match
      case r: RecordObj =>
        heapNames(r.tname) = heapNames.getOrElse(r.tname, 0) + 1
      case _ => ()
    println("[fv] initial-heap record tnames containing Completion: " +
      heapNames.filter(_._1.contains("Completion")).mkString(", "))

    // cached AST for the given source
    st.cachedAst match
      case Some(ast) =>
        val (n, s, l) = astStats(ast)
        println(s"[fv] cached AST for ${'"'}$source${'"'}: " +
          s"$n nodes ($s syntactic, $l lexical)")
        // which lexical SDOs are actually answerable on those leaves
        def leaves(a: Ast): List[Lexical] = a match
          case lex: Lexical  => List(lex)
          case syn: Syntactic => syn.children.flatten.toList.flatMap(leaves)
        val methods =
          List("StringValue", "NumericValue", "MV", "SV", "TV", "TRV")
        for (lex <- leaves(ast)) {
          val ok = methods.flatMap { m =>
            esmeta.util.BaseUtils
              .optional(esmeta.interpreter.Interpreter.eval(lex, m))
              .map(v => s"$m=$v")
          }
          println(s"[fv]   |${lex.name}|(${lex.str}) -> " +
            (if (ok.isEmpty) "(no lexical SDO)" else ok.mkString(", ")))
        }
      case None => println("[fv] no cached AST")
  }
}
