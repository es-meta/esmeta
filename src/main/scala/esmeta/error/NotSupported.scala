package esmeta.error

// not supported errors
case class NotSupported(reasons: NotSupported.ReasonPath)
  extends ESMetaError(s"${reasons.mkString("/")}", "NotSupported")
object NotSupported:
  type Reason = String
  type ReasonPath = List[Reason]
  def apply(reason: Reason): NotSupported = NotSupported(List(reason))
