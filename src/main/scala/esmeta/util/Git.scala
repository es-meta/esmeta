package esmeta.util

import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** git helpers */
abstract class Git(path: String, shortHashLength: Int = 16) { self =>
  import Git.*

  /** git versions */
  case class Version(hash: String, tag: Option[String]) {

    def git: Git = self

    /** get short hashcode */
    def shortHash: String = hash.take(shortHashLength)

    /** conversion to string */
    override def toString: String = hash + tag.fold("")(" (" + _ + ")")

    /** hash-based equality check */
    override def equals(that: Any): Boolean = that match
      case that: Version => this.git == that.git && this.hash == that.hash
      case _             => false
  }
  object Version:
    val simplePattern = "([a-z0-9]+)".r
    val tagPattern = "([a-z0-9]+) \\((.+)\\)".r
    def apply(string: String): Version = string match
      case simplePattern(hash)   => Version(hash, None)
      case tagPattern(hash, tag) => Version(hash, Some(tag))
      case _                     => throw InvalidGitVersion(string)

  /** change git version */
  def changeVersion(version: Version): Unit =
    changeVersion(version.hash)

  /** change git version with target name */
  def changeVersion(target: String): Unit =
    executeCmd(s"git checkout $target", path)

  /** get git hash */
  def getHash(target: String): String =
    executeCmd(s"git rev-list -n 1 $target", path).trim

  /** get git tag */
  def getTag(target: String): Option[String] = optional {
    executeCmd(
      s"git describe --tags --exact-match $target 2>/dev/null",
      path,
    ).trim
  }

  /** apply git patch */
  def applyPatch(patch: String): Unit = executeCmd(s"patch -p1 < $patch", path)

  /** clean modified content */
  def clean: Unit = executeCmd(s"git checkout -- .", path)

  /** default merge threshold */
  val DEFAULT_MERGE_THRESHOLD: Int = 1

  /** get git diffs between two targets */
  def getDiffs(
    from: String,
    to: String,
    target: String = ".",
    mergeThreshold: Int = DEFAULT_MERGE_THRESHOLD,
  ): List[Diff] = {
    val diffStr = executeCmd(s"git diff -U0 $from $to -- $target", path)
    val parseDiffLine = """^\s*@@ -(\d+)(,\d+)? \+(\d+)(,\d+)? @@.*$""".r
    def aux(start: String, len: String | Null): Range =
      val s = start.toInt
      val l = if (len == null) 1 else len.tail.toInt
      s until (s + l)
    val diffs = for {
      line <- diffStr.split("\n").toList
      diff <- line match {
        case parseDiffLine(x, n, y, m) => Some(Diff(aux(x, n), aux(y, m)))
        case _                         => None
      }
    } yield diff
    normalize(diffs, mergeThreshold)
  }

  /** normalize diffs by merging adjacent diffs if they are continuous */
  def normalize(
    diffs: List[Diff],
    mergeThreshold: Int = DEFAULT_MERGE_THRESHOLD,
  ): List[Diff] = {
    @annotation.tailrec
    def aux(
      diffs: List[Diff],
      revAcc: List[Diff],
    ): List[Diff] = diffs match
      case d1 :: d2 :: t
          if d1.removed.end >= d2.removed.start - mergeThreshold
          && d1.added.end >= d2.added.start - mergeThreshold =>
        val merged = Diff(
          d1.removed.start until d2.removed.end,
          d1.added.start until d2.added.end,
        )
        aux(merged :: t, revAcc)
      case d :: t => aux(t, d :: revAcc)
      case Nil    => revAcc.reverse
    aux(diffs, Nil)
  }

  /** get git commit version */
  def getVersion(targetOpt: Option[String]): Version =
    targetOpt.fold(currentVersion)(getVersion)

  /** get git commit version */
  def getVersion(target: String): Version =
    Version(getHash(target), getTag(target))

  /** get git commit hash for the current version */
  def currentVersion: Version = getVersion("HEAD")

  /** do something in a specific version */
  def getVersionWith[T](target: Option[String])(
    f: Version => T,
  ): (Version, T) = target match
    case Some(target) =>
      val cur = currentVersion
      val version = getVersion(target)
      changeVersion(version)
      val result = f(version)
      changeVersion(cur)
      (version, result)
    case None =>
      val version = currentVersion
      (version, f(version))
}
object Git {
  case class Diff(removed: Range, added: Range)
}
