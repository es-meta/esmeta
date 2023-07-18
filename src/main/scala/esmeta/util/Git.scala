package esmeta.util

import esmeta.util.SystemUtils.*

/** git helpers */
abstract class Git(path: String, shortHashLength: Int = 16) { self =>

  /** git versions */
  case class Version(name: String, hash: String) {
    def git: Git = self

    /** get short hashcode */
    def shortHash: String = hash.take(shortHashLength)

    /** conversion to string */
    override def toString: String = s"$name ($hash)"

    /** hash-based equality check */
    override def equals(that: Any): Boolean = that match
      case that: Version => this.git == that.git && this.hash == that.hash
      case _             => false
  }
  object Version:
    val pattern = "(.+) \\((.+)\\)".r
    def apply(string: String): Version = string match
      case pattern(name, hash) => Version(name, hash)

  /** change git version */
  def changeVersion(version: Version): Unit =
    changeVersion(version.hash)

  /** change git version with target name */
  def changeVersion(target: String): Unit =
    executeCmd(s"git checkout $target", path)

  /** apply git patch */
  def applyPatch(patch: String): Unit = executeCmd(s"patch -p1 < $patch", path)

  /** clean modified content */
  def clean: Unit = executeCmd(s"git checkout -- .", path)

  /** get git commit version */
  def getVersion(targetOpt: Option[String]): Version =
    targetOpt.fold(currentVersion)(getVersion)

  /** get git commit version */
  def getVersion(target: String): Version =
    val name = executeCmd(s"git name-rev --name-only $target", path).trim
    val hash = executeCmd(s"git rev-list -n 1 $target", path).trim
    Version(name, hash)

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
