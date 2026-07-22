package esmeta.injector

import esmeta.*
import esmeta.error.NoGraalError
import esmeta.es.util.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException
import scala.util.*

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  exitTag: ExitTag,
  async: Boolean,
  assertions: Vector[Assertion],
) extends InjectorElem
  with UId {

  /** replace script */
  def replaceScript(newScript: String): ConformTest = copy(script = newScript)

  /** retain only passed assertions */
  def filterAssertion: ConformTest = copy(assertions = passedAssertions)

  /** indicates if the test should exit normally */
  val isNormal: Boolean = exitTag.isNormal

  /** execute test and get result */
  lazy val (
    concreteExitTag: ExitTag,
    passedAssertions: Vector[Assertion],
    failedAssertions: Vector[(Assertion, String)],
  ) = GraalJS
    .createGraalContext { (context, out) =>
      GraalJS.runGraalUsingContext(
        script,
        context,
        Some(Engine.DEFAULT_TIMEOUT),
      )
      GraalJS.runGraalUsingContext(Injector.header, context)

      val (passes, fails) = assertions
        .map { assertion =>
          assertion -> (try
            GraalJS.runGraalUsingContextOut(
              assertion.toString,
              context,
              out,
              Some(Engine.DEFAULT_TIMEOUT),
            )
          catch {
            case _ =>
              s"An exception occured while checking this assertion.$LINE_SEP"
          })
        }
        .partition(_._2.isEmpty)
      (ExitTag.Normal, passes.map(_._1), fails)
    }
    .recoverWith {
      case error: GraalJS.JSException =>
        val msg = error.getMessage
        val tag = """\b([A-Za-z]*Error):""".r
          .findFirstMatchIn(msg)
          .map(result => ExitTag.ThrowError(result.group(1)))
          .getOrElse(ExitTag.ThrowValue(Vector(Str(msg))))
        Success((tag, Vector.empty, Vector.empty))
      case _: TimeoutException =>
        Success((ExitTag.Timeout, Vector.empty, Vector.empty))
      case error => Failure(error)
    }
    .get

  /** indicates if the expected exit tag matches the concrete exit tag */
  lazy val sameExitTag: Boolean = exitTag.equivalent(concreteExitTag)

  /** indicates if the test is passed */
  lazy val isPass: Boolean =
    try { sameExitTag && failedAssertions.isEmpty }
    catch { case NoGraalError => true }

  /** human-readable failure reason */
  lazy val msg: String =
    if (isPass) ""
    else if (!sameExitTag)
      s"[Exit Tag Mismatch]$LINE_SEP" +
      s" > Expected $exitTag but got $concreteExitTag$LINE_SEP"
    else
      failedAssertions
        .map((assertion, message) => s"$assertion$LINE_SEP > $message")
        .mkString

  /** dump this test and, when it fails, its diagnostic message */
  def dumpTest(dir: String, name: String): Unit =
    dumpFile(toString(detail = true), s"$dir/$name")
    if (!isPass) dumpFile(msg, s"$dir/$name.msg")
}
