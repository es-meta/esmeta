package esmeta.verify

import esmeta.*

sealed trait Vertice
class Hole(hole: es.Hole) extends Vertice
class Branch() extends Vertice

class OrderGraph(val edges: Set[(Vertice, Vertice)], val vertices: Set[Vertice])
  extends Graph[Vertice, (Vertice, Vertice)] {
  override def equals(that: Any): Boolean = ???
}
