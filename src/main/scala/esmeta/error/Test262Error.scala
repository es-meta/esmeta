package esmeta.error

import esmeta.LINE_SEP
import esmeta.util.Summary

sealed abstract class Test262Error(msg: String)
  extends ESMetaError(msg, s"Test262Error")

/** Test262 failure */
case class Test262Fail(fails: Summary.Elem)
  extends Test262Error(
    s"${fails.size} tests are failed:" + LINE_SEP +
    fails.all.sorted.mkString(LINE_SEP),
  )

/** asynchronous test failure reported via `$DONE` */
case class AsyncTestFailure(msg: String) extends Test262Error(msg)

/** asynchronous test not completed (`$DONE` is never called) */
case class AsyncTestIncomplete(outputs: Vector[String])
  extends Test262Error(
    "Test262:AsyncTestComplete is not printed" +
    (if (outputs.isEmpty) "" else s" (outputs: ${outputs.mkString(", ")})"),
  )
