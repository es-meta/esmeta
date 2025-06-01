package esmeta.verify

class InvokeIdCounter {
  private var count: Int = 0

  def nextId: InvokeId = {
    count += 1
    InvokeId(count)
  }
}
