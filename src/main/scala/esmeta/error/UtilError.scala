package esmeta.error

sealed abstract class UtilError(msg: String)
  extends ESMetaError(msg, "UtilError")
