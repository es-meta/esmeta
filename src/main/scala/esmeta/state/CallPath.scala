package esmeta.state

import esmeta.cfg.Call

/** abstraction of call stacks as simple paths */
case class CallPath(
  path: List[Call] = Nil,
  visited: Set[Call] = Set(),
) extends StateElem {
  def +(call: Call): CallPath =
    if (visited contains call)
      var stack = path
      var newVisited = visited
      while (
        stack match
          case head :: tail => stack = tail; newVisited -= head; head != call
          case _            => false
      ) {}
      CallPath(call :: stack, newVisited + call)
    else CallPath(call :: path, visited + call)
}
object CallPath {
  def apply(call: Call): CallPath = CallPath(List(call), Set(call))
}
