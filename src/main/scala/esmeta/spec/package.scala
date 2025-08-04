package esmeta.spec

import esmeta.spec.util.Stringifier
import esmeta.util.BaseUtils.stringify
import org.jsoup.nodes.Element

/** specification elements */
trait SpecElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}

/** extensions for Elements */
extension (elem: Element) {

  /** walker for ancestors */
  def walkAncestor[T](
    f: Element => T,
    base: T,
    join: (T, T) => T,
  ): T =
    val parent = elem.parent
    if (parent == null) base
    else join(f(parent), parent.walkAncestor(f, base, join))

  /** checks whether an element is in appendix */
  def isInAnnex: Boolean =
    elem.walkAncestor(_.tagName == "emu-annex", false, _ || _)

  /** checks whether an element is in regeular expression section */
  def isInRegexp: Boolean =
    elem.walkAncestor(
      _.id == "sec-regexp-regular-expression-objects",
      false,
      _ || _,
    )
}

/** template cases */
val templateInstances: Map[String, List[String]] = Map(
  "NativeError" -> List(
    "EvalError",
    "RangeError",
    "ReferenceError",
    "SyntaxError",
    "TypeError",
    "URIError",
  ),
  "TypedArray" -> List(
    "Int8Array",
    "Uint8Array",
    "Uint8ClampedArray",
    "Int16Array",
    "Uint16Array",
    "Int32Array",
    "Uint32Array",
    "BigInt64Array",
    "BigUint64Array",
    "Float16Array",
    "Float32Array",
    "Float64Array",
  ),
)
