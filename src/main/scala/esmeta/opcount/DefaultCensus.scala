package esmeta.opcount

import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.parser.parse as parseJson

/** For one edition, list the `$.default` sites (`DefaultCounter`, via the
  * `defaults` field of `ParseAlgs` output) that each built-in reaches through
  * its closure of invoked algorithms.
  *
  * usage: DefaultCensus <parsed.json> <closure.json> <builtins.txt> <edition>
  *   - parsed.json : `ParseAlgs` output
  *   - closure.json: {edition: {built-in: {algorithm: [steps]} or null}}
  */
object DefaultCensus {
  private def load[T: Decoder](path: String): T =
    parseJson(readFile(path)).flatMap(_.as[T]).fold(e => throw e, identity)

  def main(args: Array[String]): Unit =
    val Array(parsedPath, closurePath, builtinsPath, ed) = args
    val parsed = load[Map[String, Map[String, Json]]](parsedPath).apply(ed)
    val closure = load[Map[String, Map[String, Json]]](closurePath).apply(ed)
    val builtins = readFile(builtinsPath).linesIterator.map(_.trim).filter(_.nonEmpty).toList

    // site -> (built-ins reaching it, occurrences counting each algorithm once)
    val reach = scala.collection.mutable.Map[String, Set[String]]().withDefaultValue(Set())
    val occ = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    val algs: List[(String, String)] =
      builtins.flatMap(b => closure(b).asObject.toList.flatMap(_.keys).map(a => (b, a)))
    for ((b, alg) <- algs) {
      val sites = parsed(alg).hcursor.downField("defaults").as[Map[String, Int]]
        .fold(e => throw e, identity)
      for ((k, _) <- sites) reach(k) += b
    }
    for (alg <- algs.map(_._2).distinct) {
      val sites = parsed(alg).hcursor.downField("defaults").as[Map[String, Int]]
        .fold(e => throw e, identity)
      for ((k, n) <- sites) occ(k) += n
    }

    println(s"$ed: ${builtins.length} built-ins, ${algs.map(_._2).distinct.length} algorithms")
    println(f"${"built-ins"}%9s ${"sites"}%6s  site")
    val order = reach.keys.toList.sortBy(k => (!k.startsWith("op:"), -reach(k).size, k))
    for (k <- order) {
      val bs = reach(k).toList.sorted.map(_.replace(".prototype", ""))
      val list = if (bs.size <= 13) "  " + bs.mkString(", ") else ""
      println(f"${bs.size}%9d ${occ(k)}%6d  $k$list")
    }
}
