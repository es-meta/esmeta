package esmeta.peval

package object domain:

  abstract trait PartialElem[T, U]:
    def known: Option[T]
    def asValidForm: U
