package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.util.BaseUtils.*
import java.io.ByteArrayOutputStream
import java.time.Duration.ZERO
import java.util.StringJoiner
import java.util.concurrent.TimeoutException
import org.graalvm.polyglot.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process.*
import scala.util.*

/** ECAMScript engines */
sealed trait Engine {

  /** name of the engine */
  def name: String

  /** check if the engine can be used */
  def canUse: Boolean

  /** run ECMAScript program with timeout(ms) */
  def run(src: String, timeout: Option[Int] = None): Try[String]

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
}

/** Oracle GraalJS engine */
object GraalJS extends Engine {
  val name: String = "GraalJS"
  val command: String = "d8 --ignore-unhandled-promises -e"
  val canUse: Boolean =
    try {
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
    } catch {
      case e: Error =>
        warn(s"Unable to run GraalVM polyglot API: $e")
        false
    }
  def run(src: String, timeout: Option[Int]): Try[String] =
    runGraal(src, timeout)

  /** run JavaScript program with timeout(ms) using GraalVM Polyglot API */
  def runGraal(src: String, timeout: Option[Int] = None): Try[String] =
    createGraalContext((context, out) =>
      runGraalUsingContextOut(src, context, out, timeout),
    )

  /** run callback method using new context */
  def createGraalContext[T](f: (Context, ByteArrayOutputStream) => T): Try[T] =
    if (!canUse) throw NoGraalError
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
    timeout: Option[Int] = None,
  ): Unit =
    if (!canUse) throw NoGraalError
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
    if (!canUse) throw NoGraalError
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
}

/** ECAMScript external engines */
sealed trait ExternalEngine extends Engine {

  /** shell commands to run ECMAScript program */
  def command: String

  /** check if the engine can be used */
  def canUse: Boolean = run(";", Some(Engine.DEFAULT_TIMEOUT)) match
    case Success(value)             => true
    case Failure(NoCommandError(_)) => warn(s"No $name"); false
    case _                          => false

  /** run ECMAScript program with timeout(ms) */
  def run(src: String, timeout: Option[Int] = None): Try[String] =
    execScript(command, src, timeout)
}

/** Google V8 engine */
object D8 extends ExternalEngine {
  def name = "D8 (external)"
  def command = "d8 --ignore-unhandled-promises -e"
}

/** Oracle GraalJS engine */
object ExtGraalJS extends ExternalEngine {
  def name = "GraalJS (external)"
  def command = "js -e"
}

/** Node.js engine */
object Node extends ExternalEngine {
  def name = "Node.js (external)"
  def command = "node --unhandled-rejections=none -e"
}

object Engine {

  /** the list of available engines */
  val engines: List[Engine] = List(GraalJS, D8, ExtGraalJS, Node)

  /** default timeout for engine */
  val DEFAULT_TIMEOUT = 1_000 // 1 second
}
