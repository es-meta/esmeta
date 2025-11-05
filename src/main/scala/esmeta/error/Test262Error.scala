package esmeta.error

import esmeta.LINE_SEP
import esmeta.test262.util.Summary

sealed abstract class Test262Error(msg: String)
  extends ESMetaError(msg, s"Test262Error")

/** Test262 failure */
case class Test262Fail(fails: Summary.Elem)
  extends Test262Error(
    s"${fails.size} tests are failed:" + LINE_SEP +
    fails.all.sorted.mkString(LINE_SEP),
  )
