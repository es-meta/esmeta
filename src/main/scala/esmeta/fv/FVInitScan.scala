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
