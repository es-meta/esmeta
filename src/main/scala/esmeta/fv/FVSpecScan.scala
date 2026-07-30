package esmeta.fv

import esmeta.BASE_DIR
import esmeta.ir.*
import esmeta.ir.util.Parser
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.parser.*
import scala.collection.mutable.{Map => MMap}
import scala.util.{Failure, Success, Try}

/** Scan the compiled spec IR and report which IR constructors block
  * translation into the Rocq model, ordered by how many functions each
  * blocks.
  *
  * Input: `logs/dump/debugger/funcs.json` (produce with
  * `sbt "run dump-debugger"`), whose entries carry the function body as
  * IR concrete syntax — reparsed here with ESMeta's own parser.
  *
  * Usage:
  *   sbt "runMain esmeta.fv.FVSpecScan"                 # whole spec
  *   sbt "runMain esmeta.fv.FVSpecScan OrdinaryGet ..." # named closure
  *
  * This is a planning tool for the JS-level route: it turns "how much
  * must we model?" into an ordered, countable work list.
  */
object FVSpecScan {

  case class SpecFunc(name: String, body: Inst, main: Boolean)

  /** read funcs.json; bodies are IR concrete syntax (ir/util/JsonProtocol) */
  def readFuncs(path: String): List[SpecFunc] = {
    val json = parse(readFile(path)).getOrElse(Json.Null)
    val arr = json.asArray.getOrElse(Vector())
    arr.toList.flatMap { j =>
      val c = j.hcursor
      for {
        name <- c.downField("name").as[String].toOption
        bodyStr <- c.downField("body").as[String].toOption
        body <- Try(Inst.from(bodyStr)).toOption
        main = c.downField("main").as[Boolean].getOrElse(false)
      } yield SpecFunc(name, body, main)
    }
  }

  /** collect every construct in a body that FVExport cannot translate */
  def blockers(body: Inst): Set[String] = {
    val found = scala.collection.mutable.Set[String]()
    val walker = new esmeta.ir.util.UnitWalker {
      override def walk(e: Expr): Unit = {
        e match {
          // NOTE: ETypeCheck counts as supported only for the restricted
          // tyexp grammar of formal/Fragment.v; FVExport still validates
          // the concrete Ty and rejects anything outside it.
          case _: EParse | _: EGrammarSymbol | _: ESourceText |
              _: ESubstring | _: ETrim |
              _: EMathOp |
              _: EInstanceOf | _: ECont | _: EDebug |
              _: ERandom | _: ESyntactic | _: ELexical =>
            found += s"expr:${e.getClass.getSimpleName}"
          // COp.ToStr needs toStringHelper (Scala), so it stays UB
          case EConvert(COp.ToStr(_), _) => found += "expr:EConvert(ToStr)"
          case EMath(n) if !n.isWhole => found += "expr:EMath(non-integer)"
          case EUnary(uop, _) =>
            uop match
              case UOp.Neg | UOp.Not | UOp.Abs | UOp.Floor | UOp.BNot => ()
              case _                 => found += s"uop:$uop"
          case EBinary(bop, _, _) =>
            bop match
              case BOp.Add | BOp.Sub | BOp.Mul | BOp.Lt | BOp.Eq | BOp.And |
                  BOp.Or | BOp.Equal | BOp.Div | BOp.Mod | BOp.Pow |
                  BOp.BAnd | BOp.BOr | BOp.BXOr | BOp.LShift | BOp.RShift =>
                ()
              case _ => found += s"bop:$bop"
          case _ => ()
        }
        super.walk(e)
      }
      // all Inst forms are now covered by the Rocq model
      override def walk(i: Inst): Unit = super.walk(i)
    }
    walker.walk(body)
    found.toSet
  }

  def main(args: Array[String]): Unit = {
    val path = s"$BASE_DIR/logs/dump/debugger/funcs.json"
    if (!new java.io.File(path).exists) {
      println(s"[fv] missing $path — run: sbt \"run dump-debugger\"")
      return
    }
    val funcs = readFuncs(path)
    println(s"[fv] parsed ${funcs.size} spec functions")

    val selected =
      if (args.isEmpty) funcs
      else {
        val want = args.toSet
        funcs.filter(f => want.exists(w => f.name.contains(w)))
      }
    println(s"[fv] scanning ${selected.size} function(s)")

    val perFunc = selected.map(f => f.name -> blockers(f.body))
    val clean = perFunc.filter(_._2.isEmpty)
    println(
      f"[fv] translatable as-is: ${clean.size}%d / ${selected.size}%d " +
      f"(${100.0 * clean.size / math.max(1, selected.size)}%.0f%%)",
    )

    val hist = MMap[String, Int]()
    for ((_, bs) <- perFunc; b <- bs) hist(b) = hist.getOrElse(b, 0) + 1
    println("[fv] blocking constructs, by number of functions blocked:")
    for ((c, n) <- hist.toList.sortBy(-_._2))
      println(f"    $n%5d  $c")

    // cumulative gain: how many functions unlock as blockers are added
    val ordered = hist.toList.sortBy(-_._2).map(_._1)
    var supported = Set[String]()
    println("[fv] cumulative unlock if implemented in this order:")
    for (c <- ordered.take(20)) {
      supported += c
      val ok = perFunc.count(_._2.subsetOf(supported))
      println(f"    +$c%-34s => $ok%5d / ${selected.size}%d functions")
    }
    if (clean.nonEmpty)
      println("[fv] examples already translatable: " +
        clean.take(8).map(_._1).mkString(", "))
  }
}
