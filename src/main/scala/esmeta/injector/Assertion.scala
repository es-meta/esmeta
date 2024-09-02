package esmeta.injector

import esmeta.es.ESElem
import esmeta.state.*

/** TODO assertions from conformance test */
trait Assertion extends ESElem

case class HasValue(x: String, v: SimpleValue) extends Assertion

trait ObjectAssertion extends Assertion
case class IsExtensible(addr: Addr, path: String, extensible: Boolean = true)
  extends ObjectAssertion
case class IsCallable(addr: Addr, path: String, callable: Boolean = true)
  extends ObjectAssertion
case class IsConstructable(
  addr: Addr,
  path: String,
  constructable: Boolean = true,
) extends ObjectAssertion
case class CompareArray[T](addr: Addr, path: String, array: Iterable[T])
  extends ObjectAssertion
case class SameObject(addr: Addr, path: String, origPath: String)
  extends Assertion
case class VerifyProperty(
  addr: Addr,
  path: String,
  prop: String,
  desc: Map[String, SimpleValue],
) extends ObjectAssertion
