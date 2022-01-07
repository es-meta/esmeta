package esmeta.error

class ESMetaError(msg: String) extends Error(msg)

case class NotSupported(msg: String)
  extends ESMetaError(
    s"[NotSupported]: $msg",
  )
