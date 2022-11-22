package esmeta.error

import esmeta.VERSION

class ESMetaError(
  val errMsg: String,
  val tag: String = s"ESMeta v$VERSION",
) extends Error(s"[$tag] $errMsg")

object NoEnvVarError
  extends ESMetaError(
    "Please set the environment variable ESMETA_HOME.",
  )
