package esmeta.test262

import esmeta.TEST262_TEST_DIR
import esmeta.test262.util.*
import esmeta.util.SystemUtils.*
import scala.io.Source
import io.circe.Json
import io.circe.yaml.scalayaml.parser as yamlParser

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
    val frontmatterLines = lines.dropWhile((x) => !(x contains "/*---")) match {
      case Nil       => Nil
      case _ :: rest => rest.takeWhile((x) => !(x contains "---*/"))
    }
    val frontmatter = frontmatterLines.mkString("\n")
    val yaml: Json =
      if (frontmatter.isEmpty) then Json.Null
      else yamlParser.parse(frontmatter).getOrElse(???)

    val negative =
      yaml.hcursor.downField("negative").get[String]("type").toOption
    val es5 = yaml.hcursor.downField("es5id").succeeded
    val flags = yaml.hcursor.get[List[String]]("flags").toOption.getOrElse(Nil)
    val includes = yaml.hcursor
      .get[List[String]]("includes")
      .toOption
      .getOrElse(Nil)
      .map(_.trim)
      .filter(_.nonEmpty)
    val features =
      yaml.hcursor.get[List[String]]("features").toOption.getOrElse(Nil)
    val locales =
      yaml.hcursor.get[List[String]]("locale").toOption.getOrElse(Nil)
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
