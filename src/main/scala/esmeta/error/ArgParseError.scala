package esmeta.error

import esmeta.Command

sealed abstract class ArgParseError(msg: String)
  extends ESMetaError(msg, "ArgParseError")

case class ExtraArgError(name: String)
  extends ArgParseError(s"The option '$name' does not need an argument.")

case class NoNumArgError(name: String)
  extends ArgParseError(s"The option '$name' needs a number argument.")

case class NoStrArgError(name: String)
  extends ArgParseError(s"The option '$name' needs a string argument.")

case class NoListArgError(name: String)
  extends ArgParseError(
    s"The option '$name' needs at least one string argument.",
  )

case class NoCmdError(str: String)
  extends ArgParseError(s"Command '$str' does not exist.")

case class NoPhaseError(str: String)
  extends ArgParseError(s"Phase '$str' does not exist.")

case class NoSupportError(str: String)
  extends ArgParseError(
    s"[NoSupportError]: we do not support '$str' as an option type",
  )

case class NoObjError(str: String)
  extends ArgParseError(s"The json '$str' should be an object type.")

case class NoOptError(str: String, cmd: Command[_])
  extends ArgParseError(
    s"The option '-$str' is not available for the command '${cmd.name}'.",
  )

case class NoOptArgError(opt: String, str: String)
  extends ArgParseError(s"The option '-$opt' cannot have the value '$str'.")

case class NoFileList(str: String)
  extends ArgParseError(s"'$str' is not a file name list.")

case class NoFileName(str: String)
  extends ArgParseError(s"'$str' is not a file name.")

case class NoMode(cmd: String, mode: String)
  extends ArgParseError(s"'$cmd' command has no '$mode' mode.")
