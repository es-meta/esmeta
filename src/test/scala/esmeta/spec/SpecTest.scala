package esmeta.spec

import esmeta.ESMetaTest
import esmeta.spec.*

/** test for ECMAScript specification (ECMA-262) */
trait SpecTest extends ESMetaTest {
  def category: String = "spec"
}
