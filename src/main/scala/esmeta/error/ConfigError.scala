package esmeta.error

sealed abstract class ConfigError(msg: String) extends ESMetaError(msg)

case class OptAlreadyExistError(name: String)
  extends ConfigError(s"The option '$name' already exists in the option list.")

case object OptConflictError
  extends ConfigError(s"The option list have same options.")

case class NoChoiceError(msg: String)
  extends ConfigError(s"[NoChoiceError]: $msg")
