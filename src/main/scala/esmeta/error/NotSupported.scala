package esmeta.error

// not supported errors
case class NotSupported(msg: String) extends ESMetaError(msg, "NotSupported")
