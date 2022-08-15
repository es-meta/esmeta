package esmeta.util

import esmeta.CUR_DIR
import esmeta.util.SystemUtils.*

/** git helpers */
object Git {

  /** git versions */
  case class Version(name: String, hash: String) {
    override def toString: String = s"$name ($hash)"
  }

  /** change git version */
  def changeVersion(version: Version): Unit =
    changeVersion(version, CUR_DIR)
  def changeVersion(version: Version, dir: String): Unit =
    changeVersion(version.hash, dir)
  def changeVersion(target: String): Unit =
    changeVersion(target, CUR_DIR)
  def changeVersion(target: String, dir: String): Unit =
    executeCmd(s"git checkout $target", dir)

  /** get git commit version */
  def getVersion(target: String, dir: String = CUR_DIR): Version =
    val name = executeCmd(s"git name-rev --name-only $target", dir).trim
    val hash = executeCmd(s"git rev-parse $target", dir).trim
    Version(name, hash)

  /** get git commit hash for the current version */
  def currentVersion(dir: String = CUR_DIR): Version =
    getVersion("HEAD", dir)
}
