package esmeta.error

import esmeta.{VERSION, LINE_SEP}

class ESMetaError(
  val errMsg: String,
  val tag: String = s"ESMeta v$VERSION",
) extends Error(s"[$tag] $errMsg")

object NoEnvVarError
  extends ESMetaError(
    "Please set the environment variable ESMETA_HOME." + LINE_SEP +
    "(See https://github.com/es-meta/esmeta#environment-setting)",
  )
