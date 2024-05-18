package esmeta.es.util.delta

import scala.annotation.tailrec

/** mindd algorithm
  *
  * @param n
  * @param test
  *   returns `true` if bug remains
  * @param detail
  *   enables log
  */
class MinDD(n: Int, test: List[Boolean] => Boolean, detail: Boolean = false) {
  def result(granularity: Int = 2): List[Boolean] = {
    val list = List.range(0, n)
    val min = ddmin(list, granularity)
    log(s"done | min: $min")
    mask(min)
  }

  @tailrec private def ddmin(list: List[Int], granularity: Int): List[Int] = {
    log(s"list: $list | n: $granularity")
    if (list.size < granularity) list
    else
      val chunks = list.grouped(Math.ceilDiv(list.size, granularity)).toList
      val complement =
        chunks.find(chunk => test(mask(subtract(list, chunk))))
      complement match
        case Some(chunk) =>
          log(s"complement chunk found | chunk: $chunk")
          ddmin(subtract(list, chunk), Math.max(granularity - 1, 2))
        case None =>
          if granularity < list.size then
            log(s"increasing granularity")
            ddmin(list, Math.min(list.size, 2 * n))
          else list
  }

  // get boolean mask from index list
  private def mask(list: List[Int]): List[Boolean] = {
    var arr = List.fill(n)(false)
    for (idx <- list) arr = arr.updated(idx, true)
    arr
  }

  // get list complement of chunk
  private def subtract(list: List[Int], chunk: List[Int]): List[Int] =
    list.filter(v => !chunk.contains(v))

  private def log(x: Any): Unit = if (detail) println(s"[ddmin] $x")
}
