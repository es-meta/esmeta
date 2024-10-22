package esmeta.error

sealed abstract class ConfigError(msg: String)
  extends ESMetaError(msg, "ConfigError")

case class OptAlreadyExistError(name: String)
  extends ConfigError(s"The option '$name' already exists in the option list.")

case class OptConflictError(o1: String, o2: String)
  extends ConfigError(s"The option $o1 cannot be used with $o2.")

case class NoChoiceError(msg: String)
  extends ConfigError(s"[NoChoiceError]: $msg")

case class NoFileError(cmd: String) extends ConfigError(s"Need a file to $cmd.")
