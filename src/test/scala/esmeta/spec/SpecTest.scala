package esmeta.spec

import esmeta.ESMetaTest
import esmeta.spec.*

/** ECMAScript specifications (ECMA-262) */
trait SpecTest extends ESMetaTest {
  def category: String = "spec"
}
