package esmeta.test262

import esmeta.TEST262_TEST_DIR
import esmeta.test262.util.*
import esmeta.util.SystemUtils.*
import scala.io.Source

/** tests in Test262 */
case class Test(
  path: String,
  negative: Option[String],
  flags: List[String],
  includes: List[String],
  locales: List[String],
  features: List[String],
  es5: Boolean,
) extends Test262Elem {

  /** absolute path */
  lazy val absPath = getAbsPath(path)

  /** relative path based on TEST262_TEST_DIR */
  lazy val relName =
    if (absPath.startsWith(TEST262_TEST_DIR))
      absPath.drop(TEST262_TEST_DIR.length + 1)
    else ""
}

/** helpers of tests in Test262 */
object Test {

  /** test generation from multiple paths */
  def fromDirs(
    paths: List[String],
    features: Option[List[String]] = None,
  ): List[Test] = (for {
    path <- paths
    data <- Test.fromDir(path)
    if features.fold(true)(_.exists(data.features.contains))
  } yield data).sorted

  /** test generation from multiple a directory */
  def fromDir(dirname: String): List[Test] = walkTree(dirname).toList
    .filter(f => jsFilter(f.getName))
    .map(x => Test(x.toString))
    .sorted

  /** test generation form a single file */
  def apply(path: String): Test = {
    val source = Source.fromFile(path)
    val lines =
      try source.getLines.toList
      finally source.close()
    val test = lines.dropWhile((x) => !(x.contains("/*---"))) match {
      case Nil       => Nil
      case _ :: rest => rest.takeWhile((x) => !(x.contains("---*/")))
    }
    val (negative, flags, includes, locales, features, _, _, es5) =
      test.foldLeft(
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
          val isES5n = if (line.contains("es5id:")) true else isES5
          val isNegn = if (line.contains("negative:")) true else isNeg
          val negativen =
            if ((line.contains("type:")) && isNeg) Some(line.split(' ').last)
            else negative
          val flagsn =
            if (line.contains("flags:"))
              line
                .dropWhile(_ != '[')
                .tail
                .takeWhile(_ != ']')
                .split(", ")
                .toList
            else flags
          val (includesn, isIncluden) =
            if (line.contains("includes:")) line.dropWhile(_ != '[') match {
              case "" => (List(), true)
              case s =>
                (
                  s.tail.takeWhile(_ != ']').split(",").toList.map(_.trim),
                  isInclude,
                )
            }
            else (includes, isInclude)
          val includesn2 =
            if (isInclude && (line.contains(".js")))
              includesn :+ (line.split(' ').last)
            else includesn
          val localesn =
            if (line.contains("locale:"))
              line
                .dropWhile(_ != '[')
                .tail
                .takeWhile(_ != ']')
                .split(", ")
                .toList
            else locales
          val featuresn =
            if (line.contains("features:")) line.dropWhile(_ != '[') match {
              case "" => List()
              case s => s.tail.takeWhile(_ != ']').split(",").map(_.trim).toList
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
    Test(
      path,
      negative,
      flags,
      newIncludes,
      locales,
      features,
      es5,
    )
  }
}

/** ordering of test */
given Ordering[Test] = Ordering.by(_.path)
