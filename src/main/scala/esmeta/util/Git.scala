package esmeta.util

import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** git helpers */
abstract class Git(path: String, shortHashLength: Int = 16) { self =>

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
    println(target)
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
