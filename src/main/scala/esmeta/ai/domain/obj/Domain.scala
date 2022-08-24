package esmeta.ai.domain.obj

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract object domain */
trait Domain extends domain.Domain[Obj] {

  /** get list with abstact values */
  def getList(values: Iterable[AbsValue]): Elem

  /** get symbol with abstract description value */
  def getSymbol(desc: AbsValue): Elem

  /** object element interfaces */
  extension (elem: Elem) {

    /** lookup */
    def apply(key: AValue): AbsValue

    /** lookup */
    def get(akey: AbsValue): AbsValue

    /** get type */
    def getTy: String

    /** singleton checks */
    def isSingle: Boolean

    /** get reachable address partitions */
    def reachableParts: Set[Part]

    /** updates */
    def update(prop: AbsValue, value: AbsValue, weak: Boolean): Elem

    /** delete */
    def delete(prop: AbsValue, weak: Boolean): Elem

    /** appends */
    def append(value: AbsValue, weak: Boolean): Elem

    /** prepends */
    def prepend(value: AbsValue, weak: Boolean): Elem

    /** remove */
    def remove(value: AbsValue, weak: Boolean): Elem

    /** pops */
    def pop(weak: Boolean, front: Boolean): (AbsValue, Elem)

    /** keys of map */
    def keys(intSorted: Boolean): Elem

    /** set type of objects */
    def setType(tname: String): Elem

    /** check contains */
    def contains(value: AbsValue): AbsValue
  }
}
