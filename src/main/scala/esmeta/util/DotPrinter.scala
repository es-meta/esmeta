package esmeta.util

object DotProtocol:

  // • Primitive instance
  given intDot: DotProtocol[Int] with
    def toDotString(n: Int): String = n.toString

  // • Recursive instance (list pretty-prints its elements)
  given listDot[A](using ev: DotProtocol[A]): DotProtocol[List[A]] with
    def toDotString(xs: List[A]): String =
      xs.map(ev.toDotString).mkString("[", ", ", "]")

  // • Custom case-class instance
  final case class Person(name: String, age: Int)

  given personDot: DotProtocol[Person] with
    def toDotString(p: Person): String =
      s""""${p.name}" [label="${p.name}\\n${p.age}"];"""

object dotSyntax:
  extension [T: DotProtocol](value: T)
    /** Syntactic sugar: `x.toDotString` instead of `DotProtocol[...]` dance. */
    def printDot: String = summon[DotProtocol[T]].toDotString(value)

// works in both Scala 3 and Scala 2.x
trait DotProtocol[T] {

  /** Render `value` in Graphviz “dot” syntax (or whatever you need). */
  def toDotString(value: T): String
}

@main def demo(): Unit =
  import DotProtocol.* // bring the given/implicit instances in
  import dotSyntax.* // bring the extension method in

  println(42.printDot) // → 42
  println(List(1, 2, 3).printDot) // → [1, 2, 3]

  val alice = DotProtocol.Person("Alice", 30)
  println(alice.printDot)
