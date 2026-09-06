package esmeta.es

/** ECMAScript script program */
case class Script(code: String, name: String, supported: Boolean = true)
  extends ESElem
