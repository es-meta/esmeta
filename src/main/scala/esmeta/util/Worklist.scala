package esmeta.util

import scala.collection.mutable.{Stack, Queue, PriorityQueue}

// worklist
trait Worklist[T] {
  protected var set = Set[T]()
  protected def add(x: T): Unit
  protected def pop: T
  def clear: Unit
  def reset(xs: Iterable[T]): this.type = { clear; xs.foreach(this += _); this }
  def all: Set[T] = set
  def +=(x: T): Unit = if (!set.contains(x)) { add(x); set += x }
  def ++=(xs: Iterable[T]): Unit = xs.foreach(this += _)
  def next: Option[T] =
    if (isEmpty) None
    else { val x = pop; set -= x; Some(x) }
  inline def size: Int = set.size
  inline def isEmpty: Boolean = all.isEmpty
  inline def nonEmpty: Boolean = !isEmpty
  def foreach(f: T => Unit): Unit
  def has(x: T): Boolean = all contains x
}

// stack-based worklist
class StackWorklist[T] extends Worklist[T] {
  private var stack = Stack[T]()
  def clear: Unit = { set = Set(); stack.clear }
  protected def add(x: T): Unit = stack.push(x)
  protected def pop: T = stack.pop
  def foreach(f: T => Unit): Unit = stack.foreach(f)
}
object StackWorklist:
  def apply[T](xs: Iterable[T]) = new StackWorklist[T].reset(xs)

// queue-based worklist
class QueueWorklist[T] extends Worklist[T] {
  private var queue = Queue[T]()
  def clear: Unit = { set = Set(); queue.clear }
  protected def add(x: T): Unit = queue.enqueue(x)
  protected def pop: T = queue.dequeue
  def foreach(f: T => Unit): Unit = queue.foreach(f)
}
object QueueWorklist:
  def apply[T](xs: Iterable[T]) = new QueueWorklist[T].reset(xs)

// priority-queue-based worklist
class PriorityQueueWorklist[T](using ord: Ordering[T]) extends Worklist[T] {
  private var pq = PriorityQueue[T]()(ord.reverse)
  def clear: Unit = { set = Set(); pq.clear }
  protected def add(x: T): Unit = pq.enqueue(x)
  protected def pop: T = pq.dequeue
  def foreach(f: T => Unit): Unit =
    val all = pq.dequeueAll
    all.foreach(f)
    pq ++= all
}
object PriorityQueueWorklist:
  def apply[T](xs: Iterable[T])(using ord: Ordering[T]) =
    new PriorityQueueWorklist[T].reset(xs)
