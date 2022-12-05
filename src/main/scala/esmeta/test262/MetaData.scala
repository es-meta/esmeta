package esmeta.test262

import esmeta.TEST262_TEST_DIR
import esmeta.test262.util.*
import esmeta.util.SystemUtils.*
import scala.io.Source

/** metadata tests in Test262 */
case class MetaData(
  relName: String,
  path: String,
  negative: Option[String],
  flags: List[String],
  includes: List[String],
  locales: List[String],
  features: List[String],
  es5: Boolean,
) extends Test262Elem

/** helpers of metadata tests in Test262 */
object MetaData {

  /** metadata generation from multiple paths */
  def fromDirs(paths: List[String]): List[MetaData] = (for {
    path <- paths
    data <- MetaData.fromDir(path)
  } yield data).sorted

  /** metadata generation from multiple a directory */
  def fromDir(dirname: String): List[MetaData] = walkTree(dirname).toList
    .filter(f => jsFilter(f.getName))
    .map(x => MetaData(x.toString))
    .sorted

  /** metadata generation form a single file */
  def apply(filename: String): MetaData = {
    val absPath = getAbsPath(filename)
    val relName =
      if (absPath.startsWith(TEST262_TEST_DIR))
        absPath.drop(TEST262_TEST_DIR.length + 1)
      else ""
    val source = Source.fromFile(filename)
    val lines =
      try source.getLines.toList
      finally source.close()
    val metadata = lines.dropWhile((x) => !(x contains "/*---")) match {
      case Nil       => Nil
      case _ :: rest => rest.takeWhile((x) => !(x contains "---*/"))
    }
    val (negative, flags, includes, locales, features, _, _, es5) =
      metadata.foldLeft(
        (
          Option.empty[String],
          List[String](),
          List[String](),
          List[String](),
          List[String](),
          false,
          false,
          false,
        ),
      ) {
        case (
              (
                negative,
                flags,
                includes,
                locales,
                features,
                isNeg,
                isInclude,
                isES5,
              ),
              line,
            ) => {
          val isES5n = if (line contains "es5id:") true else isES5
          val isNegn = if (line contains "negative:") true else isNeg
          val negativen =
            if ((line contains "type:") && isNeg) Some(line.split(' ').last)
            else negative
          val flagsn =
            if (line contains "flags:")
              line
                .dropWhile(_ != '[')
                .tail
                .takeWhile(_ != ']')
                .split(", ")
                .toList
            else flags
          val (includesn, isIncluden) =
            if (line contains "includes:") line.dropWhile(_ != '[') match {
              case "" => (List(), true)
              case s =>
                (
                  s.tail.takeWhile(_ != ']').split(",").toList.map(_.trim),
                  isInclude,
                )
            }
            else (includes, isInclude)
          val includesn2 =
            if (isInclude && (line contains ".js"))
              includesn :+ (line.split(' ').last)
            else includesn
          val localesn =
            if (line contains "locale:")
              line
                .dropWhile(_ != '[')
                .tail
                .takeWhile(_ != ']')
                .split(", ")
                .toList
            else locales
          val featuresn =
            if (line contains "features:") line.dropWhile(_ != '[') match {
              case "" => List()
              case s  => s.tail.takeWhile(_ != ']').split(", ").toList
            }
            else features
          (
            negativen,
            flagsn,
            includesn2,
            localesn,
            featuresn,
            isNegn,
            isIncluden,
            isES5n,
          )
        }
      }
    val newIncludes =
      if (flags contains "async") includes :+ "doneprintHandle.js" else includes
    MetaData(
      relName,
      filename,
      negative,
      flags,
      newIncludes,
      locales,
      features,
      es5,
    )
  }
}

/** ordering of metadata */
given Ordering[MetaData] = Ordering.by(_.path)

/** extensions for list of metadata */
extension (data: List[MetaData]) {

  /** Remove metadata using filters. It returns tuple, first element being list
    * after removal, second element being map with key of a feature and value of
    * list being removed by the feature.
    */
  def remove(
    pairs: (String, MetaData => Boolean)*,
  ): (List[MetaData], Map[String, List[MetaData]]) =
    val removedMap = scala.collection.mutable.Map[String, List[MetaData]]()
    (
      pairs.foldLeft(data) {
        case (data, (desc, f)) =>
          val (filtered, removed) = data.foldLeft(List[MetaData](), 0) {
            case ((l, count), meta) =>
              if (f(meta)) then
                removedMap(desc) =
                  meta :: removedMap.getOrElse(desc, List[MetaData]())
                (l, count + 1)
              else (meta :: l, count)
          }
          if (removed > 0)
            println(f"- $desc%-30s: $removed%,5d tests are removed")
          filtered.reverse
      },
      removedMap.map((k, v) => (k, v.reverse)).toMap,
    )

  /** get the summary */
  def summary: ConfigSummary =
    val (normalL, errorL) = data.partition(_.negative.isEmpty)
    val n = normalL.length
    val e = errorL.length
    val t = n + e
    if (t > 1)
      println(s"----------------------------------------")
      println(f"- total: $t%,d available tests")
      println(f"  - normal: $n%,d tests")
      println(f"  - error: $e%,d tests")
      println(s"----------------------------------------")
    ConfigSummary(
      data,
      normalL.map(d => NormalConfig(d.path, d.includes)),
      errorL.map(d => ErrorConfig(d.path, d.negative.get, d.includes)),
    )
}
