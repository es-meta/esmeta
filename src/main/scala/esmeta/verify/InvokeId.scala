package esmeta.verify

case class InvokeId(id: Int) extends Ordered[InvokeId] {
  override def compare(that: InvokeId): Int = this.id.compareTo(that.id)
  override def toString: String = s"InvokeId($id)"

  def next(): InvokeId = InvokeId(id + 1)
}
