package esmeta.util

import esmeta.error.WrongUId

/** unique ids */
trait UId[T <: UId[T]] { this: T =>
  // assign unique ids
  val gen: UId.Gen[T]

  // assign unique ids
  var uid: Int = gen.newId

  // store this to uidMap
  gen.uidMap += uid -> this

  // get simple string
  def uidString: String = s"${getClass.getSimpleName}[$uid]"

  // override equality comparison using unique ids
  override def equals(that: Any): Boolean = that match {
    case that: UId[_] => (
      (this.gen eq that.gen) &&
      (this.uid == that.uid)
    )
    case _ => false
  }

  // override hashCode using unique ids
  override def hashCode: Int = uid
}
object UId:
  // unique id generator
  trait Gen[T <: UId[T]] {
    // private uid counter
    private[util] var count = 0
    private[util] def newId: Int = { val uid = count; count += 1; uid }
    private[util] var uidMap: Map[Int, T] = Map()
    def get(uid: Int): T = uidMap.getOrElse(uid, throw WrongUId(toString, uid))
    def size: Int = count
  }
