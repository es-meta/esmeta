package esmeta.js

import java.io.ByteArrayOutputStream
import java.util.StringJoiner
import java.time.Duration.ZERO
import scala.util.*
import scala.concurrent.*
import scala.sys.process.*
import scala.concurrent.ExecutionContext.Implicits.global
import org.graalvm.polyglot.*
import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.util.BaseUtils.*

/** JavaScript Engines */
object JSEngine {

  /** default commands */
  val defaultCmd = Map(
    "js" -> "js -e", // graal.js
    "node" -> "node --unhandled-rejections=none -e", // node.js
  )

  /** Check if GraalVM polyglot API can be used */
  lazy val useGraal: Boolean =
    try
      Using(Context.newBuilder("js").build()) { context =>
        try {
          context.eval("js", "")
        } catch
          case e =>
            // TODO(@hyp3rflow): fix this error message correctly
            warn(s"Unable to run GraalVM polyglot API: $e")
            throw e
      }.isSuccess
    catch {
      case e: Error =>
        // TODO(@hyp3rflow): fix this error message correctly
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
    Using(Context.newBuilder("js").out(out).build()) { context =>
      f(context, out)
    }.recoverWith(e => polyglotExceptionResolver(e))

  /** execute given JavaScript program */
  def runGraalUsingContext(
    src: String,
    context: Context,
    timeout: Option[Int] = None,
  ): Unit =
    if (!useGraal) throw NoGraalError
    val stat = Status()
    timeout.foreach(millis => registerTimeout(context, millis, stat))
    stat.running = true
    try context.eval("js", src)
    finally stat.done = true

  /** execute given JavaScript program and return its stdout as string */
  def runGraalUsingContextOut(
    src: String,
    context: Context,
    out: ByteArrayOutputStream,
    timeout: Option[Int] = None,
  ): String =
    if (!useGraal) throw NoGraalError
    val stat = Status()
    out.reset
    timeout.foreach(millis => registerTimeout(context, millis, stat))
    stat.running = true
    try {
      context.eval("js", src)
      out.toString
    } finally {
      context.close
      stat.done = true
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

  case class Status(var running: Boolean = false, var done: Boolean = false)

  /** register timeout to context in milliseconds */
  def registerTimeout(context: Context, timeout: Int, stat: Status) =
    Future {
      while (!stat.done) {
        Thread.sleep(timeout)
        if (!stat.done && stat.running)
          // TODO race condition:
          // new eval is performed between
          // (!stat.done) check and interrupt
          context.interrupt(ZERO)
      }
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
}
