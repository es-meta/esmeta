package esmeta.util.domain

import esmeta.util.BaseUtils.*
import Flat.*, BSet.*

/** flat abstract domain */
trait FlatDomain[A] extends Domain[A, Flat[A]]:
  val Top = Many
  val Bot = Zero
  def alpha(elems: Iterable[A]): Flat[A] = Flat(elems)

enum Flat[+A]:
  case Many extends Flat[Nothing]
  case One(value: A) extends Flat[A]
  case Zero extends Flat[Nothing]

object Flat:
  val Top = Many
  val Bot = Zero
  inline def apply[A](elems: A*): Flat[A] = apply(elems)
  def apply[A](elems: Iterable[A]): Flat[A] =
    if (elems.isEmpty) Zero
    else if (elems.size == 1) One(elems.head)
    else Many

given flatOps[A]: Ops[A, Flat[A]] with
  extension (elem: Flat[A])
    def isTop: Boolean = elem == Many
    def isBottom: Boolean = elem == Zero
    def ⊑(that: Flat[A]): Boolean = (elem, that) match
      case (Zero, _) | (_, Many) => true
      case (Many, _) | (_, Zero) => false
      case (One(l), One(r))      => l == r
    def ⊔(that: Flat[A]): Flat[A] = (elem, that) match
      case (Many, _) | (_, Many) => Many
      case (Zero, _)             => that
      case (_, Zero)             => elem
      case (One(l), One(r))      => if (l == r) elem else Many
    def ⊓(that: Flat[A]): Flat[A] = (elem, that) match
      case (Zero, _) | (_, Zero) => Zero
      case (Many, _)             => that
      case (_, Many)             => elem
      case (One(l), One(r))      => if (l == r) elem else Zero
    def --(that: Flat[A]): Flat[A] = (elem, that) match
      case (Zero, _) | (_, Many) => Zero
      case (_, Zero)             => elem
      case (Many, One(_))        => Many
      case (One(l), One(r))      => if (l == r) Zero else elem
    def contains(value: A): Boolean = elem match
      case Many   => true
      case One(v) => v == value
      case Zero   => false
    def gamma: BSet[A] = elem match
      case Many       => Inf
      case One(value) => Fin(Set(value))
      case Zero       => Fin(Set())
    def getSingle: Flat[A] = elem

    /** map function */
    def map[B](f: A => B): Flat[B] = elem match
      case Many       => Many
      case One(value) => One(f(value))
      case Zero       => Zero

    /** prune operator */
    def --(that: => Flat[A]): Flat[A] = (elem, that) match
      case (Many, One(_))        => Many
      case (_, Zero)             => elem
      case (One(l), One(r))      => if (l == r) Zero else elem
      case (Zero, _) | (_, Many) => Zero
