package esmeta.error

sealed abstract class UtilError(msg: String)
  extends ESMetaError(msg, "UtilError")

case class InvalidGitVersion(msg: String)
  extends UtilError(s"Invalid git version: $msg")

case class GitTagMismatch(hash: String, tagName: String)
  extends UtilError(s"Git tag mismatch: $hash != $tagName")

case class NoCommandError(command: String)
  extends UtilError(s"Command not found: $command")
