package esmeta.error

sealed abstract class UtilError(msg: String) extends ESMetaError(msg)

case class WrongUId(name: String, uid: Int)
  extends UtilError(s"[WrongUId] uid $uid does not exist in $name.")
