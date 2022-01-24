package esmeta.util

import esmeta.error.WrongUId

/** unique ids */
trait UId {
  // unique ids
  val id: Int

  // get simple string
  def simpleString: String = s"${getClass.getSimpleName}[$id]"

  // override equality comparison using unique ids
  override def equals(that: Any): Boolean = that match {
    case that: UId => (
      (this.getClass eq that.getClass) &&
      (this.id == that.id)
    )
    case _ => false
  }

  // override hashCode using unique ids
  override def hashCode: Int = id
}
