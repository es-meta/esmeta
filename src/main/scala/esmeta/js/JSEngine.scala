package esmeta.js

import java.io.ByteArrayOutputStream
import java.util.StringJoiner
import java.util.concurrent.TimeoutException
import java.time.Duration.ZERO
import scala.util.*
import scala.concurrent.*
import scala.sys.process.*
import scala.concurrent.ExecutionContext.Implicits.global
import org.graalvm.polyglot.*
import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.atomic.AtomicReference

/** JavaScript Engines */
object JSEngine {

  /** default commands */
  val defaultCmd = Map(
    "d8" -> "d8 --ignore-unhandled-promises -e", // d8
    "js" -> "js -e", // graal.js
    "node" -> "node --unhandled-rejections=none -e", // node.js
  )

  /** Check if GraalVM polyglot API can be used */
  lazy val useGraal: Boolean =
    try
      Using(
        Context
          .newBuilder("js")
          .option("engine.WarnInterpreterOnly", "false")
          .build(),
      ) { context =>
        try {
          context.eval("js", "")
        } catch
          case e =>
            warn(s"Unable to run GraalVM polyglot API: $e")
            throw e
      }.isSuccess
    catch {
      case e: Error =>
        warn(s"Unable to run GraalVM polyglot API: $e")
        false
    }

  /** run JavaScript program with timeout(ms) using GraalVM Polyglot API */
  def runGraal(src: String, timeout: Option[Int] = None): Try[String] =
    createGraalContext((context, out) =>
      runGraalUsingContextOut(src, context, out, timeout),
    )

  /** run callback method using new context */
  def createGraalContext[T](
    f: (Context, ByteArrayOutputStream) => T,
  ): Try[T] =
    if (!useGraal) throw NoGraalError
    val out = new ByteArrayOutputStream
    Using(
      Context
        .newBuilder("js")
        .option("engine.WarnInterpreterOnly", "false")
        .out(out)
        .build(),
    ) { context =>
      f(context, out)
    }.recoverWith(e => polyglotExceptionResolver(e))

  /** execute given JavaScript program */
  def runGraalUsingContext(
    src: String,
    context: Context,
    limit: Option[Int] = None,
  ): Unit =
    if (!useGraal) throw NoGraalError
    try {
      timeout(context.eval("js", src), limit)
    } finally {
      context.close
    }

  /** execute given JavaScript program and return its stdout as string */
  def runGraalUsingContextOut(
    src: String,
    context: Context,
    out: ByteArrayOutputStream,
    limit: Option[Int] = None,
  ): String =
    if (!useGraal) throw NoGraalError
    out.reset
    try {
      timeout(
        {
          context.eval("js", src)
          out.toString
        },
        limit,
      )
    } finally {
      context.close
    }

  // ---------------------------------------------------------------------------
  // handling PolyglotException from Polyglot API
  // ---------------------------------------------------------------------------

  /** Exception from JavaScript code */
  class JSException(message: String) extends Exception(message)

  /** resolve PolyglotException to other exceptions */
  def polyglotExceptionResolver[T](e: Throwable): Try[T] = e match {
    case e: PolyglotException if (e.isInterrupted || e.isCancelled) =>
      Failure(TimeoutException("JSEngine timeout"))
    case e: PolyglotException if e.isGuestException =>
      Failure(JSException(e.getMessage))
    case e: PolyglotException if e.isHostException =>
      Failure(e.asHostException)
    case _ =>
      Failure(e)
  }

  // -------------------------------------------------------------------------
  // executing JavaScript program using shell command
  // -------------------------------------------------------------------------
  def execScript(
    command: String,
    src: String,
    timeout: Option[Int] = None,
  ): Try[String] = Try {
    val escapedSrc = escapeToShellString(src)
    val stdout = new StringJoiner(LINE_SEP)
    val stderr = new StringJoiner(LINE_SEP)
    def cmd(main: String) = timeout match
      case Some(timeout) => s"timeout ${timeout / 1000f}s $main $escapedSrc"
      case None          => s"$main $escapedSrc"
    val pb: ProcessBuilder = if command.contains("|") then {
      val Array(main, envInfo) = command.split("\\|")
      val Array(envKey, envVal) = envInfo.split(":")
      Process(cmd(main), None, envKey -> envVal)
    } else cmd(command)

    pb ! ProcessLogger(
      out => stdout.add(out),
      err => stderr.add(err),
    ) match {
      case 0         => stdout.toString
      case 124 | 137 => throw TimeoutException(command)
      case 127       => throw NoCommandError(command)
      case st        => throw new Exception(stdout.toString + stderr.toString)
    }
  }

  lazy val useJs: Boolean =
    runJs(";", Some(1000)) match
      case Success(value)             => true
      case Failure(NoCommandError(_)) => warn("No Graal.js"); false
      case _                          => false

  def runJs(src: String, timeout: Option[Int] = None): Try[String] =
    execScript(defaultCmd("js"), src, timeout)

  lazy val useNode: Boolean =
    runNode(";", Some(1000)) match
      case Success(value)             => true
      case Failure(NoCommandError(_)) => warn("No Node.js"); false
      case _                          => false

  def runNode(src: String, timeout: Option[Int] = None): Try[String] =
    execScript(defaultCmd("node"), src, timeout)

  lazy val useD8: Boolean =
    runD8(";", Some(1000)) match
      case Success(value)             => true
      case Failure(NoCommandError(_)) => warn("No D8"); false
      case _                          => false

  def runD8(src: String, timeout: Option[Int] = None): Try[String] =
    execScript(defaultCmd("d8"), src, timeout)
}
