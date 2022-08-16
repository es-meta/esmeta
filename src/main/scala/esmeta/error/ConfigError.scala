package esmeta.error

sealed abstract class ConfigError(msg: String)
  extends ESMetaError(msg, "ConfigError")

case class OptAlreadyExistError(name: String)
  extends ConfigError(s"The option '$name' already exists in the option list.")

case object OptConflictError
  extends ConfigError(s"The option list have same options.")

case class NoChoiceError(msg: String)
  extends ConfigError(s"[NoChoiceError]: $msg")

case class NoFileError(cmd: String) extends ConfigError(s"Need a file to $cmd.")
