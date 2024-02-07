package esmeta.analyzer.domain.obj

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ObjDomainDecl { self: Self =>

  /** abstract object domain */
  trait ObjDomain extends Domain[Obj] {

    /** get list with abstract values */
    def getList(values: Iterable[AbsValue] = Nil): Elem

    /** get list with a merged abstract value */
    def getMergedList(value: AbsValue): Elem

    /** get symbol with abstract description value */
    def getSymbol(desc: AbsValue): Elem

    /** object element interfaces */
    extension (elem: Elem) {

      /** lookup */
      def apply(key: AValue): AbsValue

      /** lookup */
      def get(akey: AbsValue): AbsValue

      /** get list with abstract values */
      def getList: Option[Vector[AbsValue]]

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

      /** concat */
      def concat(list: AbsObj, weak: Boolean): Elem

      /** duplicated element check */
      def duplicated: AbsBool

      /** append */
      def append(value: AbsValue, weak: Boolean): Elem

      /** prepend */
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

      /** find merged parts */
      def findMerged(
        part: Part,
        path: String,
        aux: (AbsValue, String, String) => Unit,
      ): Unit
    }
  }
}
