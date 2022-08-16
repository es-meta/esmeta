package esmeta.error

class ESMetaError(
  val errMsg: String,
  val tag: String = "ESMetaError",
) extends Error(s"[$tag] $errMsg")
