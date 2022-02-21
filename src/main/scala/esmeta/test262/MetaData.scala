package esmeta.test262

import esmeta.TEST262_TEST_DIR
import esmeta.test262.util.*
import scala.io.Source

/** metadata tests in Test262 */
case class MetaData(
  name: String,
  negative: Option[String],
  flags: List[String],
  includes: List[String],
  locales: List[String],
  features: List[String],
  es5: Boolean,
) extends Test262Elem

object MetaData {
  def apply(filename: String): MetaData = {
    val relName =
      if (!filename.startsWith(TEST262_TEST_DIR)) filename
      else filename.drop(TEST262_TEST_DIR.length + 1)
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
      negative,
      flags,
      newIncludes,
      locales,
      features,
      es5,
    )
  }
}
