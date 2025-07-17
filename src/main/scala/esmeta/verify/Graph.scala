package esmeta.verify

trait Graph[V, E] {
  val edges: Set[E]
  val vertices: Set[V]
}
