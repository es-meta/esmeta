package esmeta.spec

import esmeta.lang.NumericLiteral

/** constants defined by `emu-eqn` elements in ECMA-262 */
case class Constant(name: String, value: NumericLiteral) extends SpecElem
