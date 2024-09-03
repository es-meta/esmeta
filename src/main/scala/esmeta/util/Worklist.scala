package esmeta.util

import scala.collection.mutable.{Stack, Queue, PriorityQueue}

// worklist
trait Worklist[T] {
  protected var set = Set[T]()
  protected def add(x: T): Unit
  protected def pop: T
  def all: Set[T] = set
  def +=(x: T): Unit = if (!set.contains(x)) { add(x); set += x }
  def ++=(xs: Iterable[T]): Unit = xs.foreach(this += _)
  def next: Option[T] =
    if (isEmpty) None
    else { val x = pop; set -= x; Some(x) }
  def size: Int = set.size
  def isEmpty: Boolean = all.isEmpty
  def foreach(f: T => Unit): Unit
  def has(x: T): Boolean = all contains x
}

// stack-based worklist
class StackWorklist[T](init: Iterable[T]) extends Worklist[T] {
  private var stack = Stack[T]()
  init.foreach(this += _)

  protected def add(x: T): Unit = stack.push(x)
  protected def pop: T = stack.pop
  def foreach(f: T => Unit): Unit = stack.foreach(f)
}

// queue-based worklist
class QueueWorklist[T](init: Iterable[T]) extends Worklist[T] {
  private var queue = Queue[T]()
  init.foreach(this += _)

  protected def add(x: T): Unit = queue.enqueue(x)
  protected def pop: T = queue.dequeue
  def foreach(f: T => Unit): Unit = queue.foreach(f)
}

// priority-queue-based worklist
class PriorityQueueWorklist[T](init: Iterable[T])(using ord: Ordering[T])
  extends Worklist[T] {
  private var pq = PriorityQueue[T]()(ord.reverse)
  init.foreach(this += _)

  protected def add(x: T): Unit = pq.enqueue(x)
  protected def pop: T = pq.dequeue
  def foreach(f: T => Unit): Unit = pq.foreach(f)
}
