package esmeta.es

import esmeta.*
import esmeta.error.*
import esmeta.util.SystemUtils.readFile
import scala.util.{Success, Failure, Try}
import sys.process._
import esmeta.util.BaseUtils.*
import java.util.StringJoiner
import esmeta.es.util.USE_STRICT
import esmeta.LINE_SEP

import sttp.client4.*
import java.net.URLEncoder
import sttp.model.StatusCode

/** JavaScript Transpiler utilities */
object JSTrans {
  val transpileCmd = Map(
    "swc" -> "minify-runner -v swc@1.3.10",
    "swcES2015" -> "minify-runner -v swc@1.3.10 --notcompress -t es2015",
    "terser" -> "minify-runner -v terser@5.15.1",
    "babel" -> "minify-runner -v babel@7.19.1",
    "checkDiffSwc" -> "minify-runner -v swc@1.3.10 -d",
    "checkDiffSwcES2015" -> "minify-runner -v swc@1.3.10 --notcompress -t es2015 -d",
    "checkDiffTerser" -> "minify-runner -v terser@5.15.1 -d",
    "checkDiffBabel" -> "minify-runner -v babel@7.19.1 -d",
  )

  private var hasWarned = false

  def warnUnspecified(): Unit =
    if !hasWarned then
      println("No transpiler specified. Using SWC as default.")
      hasWarned = true

  def execScript(
    command: String,
    src: String,
    timeout: Option[Int] = None,
  ): Try[String] = Try {
    val escapedSrc = escapeToShellString(src)
    val stdout = new StringJoiner(LINE_SEP)
    val stderr = new StringJoiner(LINE_SEP)
    def cmd(main: String) = timeout match
      case Some(timeout) => s"timeout ${timeout}s $main $escapedSrc"
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

  def transpile(src: String): Try[String] =
    transpile(src, None)

  def transpile(src: String, cmd: Option[String]): Try[String] =
    val transpilerCode = cmd match
      case Some("swc") | Some("Swc")             => "swc"
      case Some("terser") | Some("Terser")       => "terser"
      case Some("babel") | Some("Babel")         => "babel"
      case Some("swcES2015") | Some("SwcES2015") => "swcES2015"
      case None =>
        warnUnspecified()
        "swc"
      case _ => throw new Exception("Invalid transpiler specified.")
    execScript(transpileCmd(transpilerCode), src)

  def transpileSrv(src: String, cmd: Option[String]): Try[String] =
    JSTransServer.query(src, cmd, diff = false) match {
      case "none" => Failure(new Exception("transpile failure"))
      case result => Success(result)
    }

  def checkTranspileDiff(code: String, cmd: Option[String]): Boolean =
    val transpilerCode = cmd match
      case Some("swc") | Some("Swc")             => "checkDiffSwc"
      case Some("terser") | Some("Terser")       => "checkDiffTerser"
      case Some("babel") | Some("Babel")         => "checkDiffBabel"
      case Some("swcES2015") | Some("SwcES2015") => "checkDiffSwcES2015"
      case None =>
        warnUnspecified()
        "checkDiffSwc"
      case _ => throw new Exception("Invalid transpiler specified.")
    try {
      val result = execScript(transpileCmd(transpilerCode), code)
      result match {
        case Success(transpiledAndDiff) =>
          val diffResult = transpiledAndDiff.split(LINE_SEP).last
          if diffResult == "true" then true
          else if diffResult == "false" then false
          else {
            throw new Exception(s"Invalid diff result: $diffResult")
          }
        case Failure(exception) =>
          // println(s"[minify-check] $code $exception")
          false
      }
    } catch {
      case err => false
    }

  def checkTranspileDiffSrv(code: String, cmd: Option[String]): Boolean =
    checkTranspileDiffSrvOpt(code, cmd).getOrElse(false)

  def checkTranspileDiffSrvOpt(
    code: String,
    cmd: Option[String],
  ): Option[Boolean] =
    try {
      val diffResult =
        JSTransServer.query(code, cmd, diff = true).split(LINE_SEP).last
      if diffResult == "true" then Some(true)
      else if diffResult == "false" then Some(false)
      else if diffResult == "none" then None
      else {
        throw new Exception(s"Invalid diff result: $diffResult")
      }
    } catch {
      case err => None
    }

  /** indicating the result of transpilation was faillure */
  val failTag = "TRANSPILE_FAILURE"

}

object JSTransServer {
  val port = 8282
  val serverUrl = s"http://127.0.0.1:$port/"

  val backend = DefaultSyncBackend()

  def query(code: String, cmd: Option[String], diff: Boolean): String = {
    val version = cmd match
      case Some("swc") | Some("Swc") | Some("swcES2015") | Some("SwcES2015") =>
        "swc@1.3.10"
      case Some("terser") | Some("Terser") => "terser@5.15.1"
      case Some("babel") | Some("Babel")   => "babel@7.19.1"
      case None =>
        JSTrans.warnUnspecified()
        "swc@1.3.10"
      case _ => throw new Exception("Invalid minifier specified.")

    val encodedCode = URLEncoder.encode(code, "UTF-8")

    val additionalOptions = cmd match
      case Some("swcES2015") | Some("SwcES2015") =>
        "&notcompress=true&target=es2015"
      case _ => ""
    val url =
      s"$serverUrl?codeOrFilePath=$encodedCode&version=$version" +
      (if diff then "&diff=true" else "") + additionalOptions
    val request = basicRequest.get(uri"$url")

    val response = request.send(backend)
    val endTime = System.currentTimeMillis()

    response.body match {
      case Right(output) => output
      case Left(error) =>
        if response.code == StatusCode(400) then
          println(s"MinifyServer Error: ${response.code} $error, with $code");
          "none"
        else "none"
    }
  }

}
