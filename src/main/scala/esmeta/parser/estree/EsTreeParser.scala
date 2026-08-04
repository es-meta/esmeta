package esmeta.parser.estree

import esmeta.error.*
import io.circe.*, io.circe.syntax.*
import java.io.*
import java.nio.file.{Files, Path, StandardCopyOption}

/** ESTree parser backed by a one-shot Node.js script
  *
  * Every request spawns a fresh Node.js process, writes a single JSON request
  * to its stdin, and reads a single JSON response back. Spawning costs tens of
  * milliseconds, which is negligible against the seconds the reference parser
  * spends on the long files this parser exists for.
  */
object EsTreeParser {

  /** the Node.js executable */
  val NODE = sys.env.getOrElse("ESMETA_NODE", "node")

  /** parse a code string */
  def from(code: String, sourceType: String = "script"): EsTree =
    request(
      Json.obj(
        "code" -> code.asJson,
        "sourceType" -> sourceType.asJson,
      ),
    )

  /** check whether the ESTree parser is usable */
  lazy val canUse: Boolean =
    try { from("0;"); true }
    catch { case _: Throwable => false }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // The parsing script and the acorn bundle it imports are resources, so they
  // are unpacked side by side into a temporary directory for Node.js to run.
  private lazy val script: Path =
    val dir = Files.createTempDirectory("esmeta-estree")
    // the directory is registered first, so that it is removed after the files
    // it holds: deletion hooks run in reverse order of registration
    dir.toFile.deleteOnExit()
    for (name <- List("parse.mjs", "acorn.mjs")) unpack(name, dir)
    dir.resolve("parse.mjs")

  private def unpack(name: String, dir: Path): Unit =
    val in = getClass.getResourceAsStream(s"/estree/$name")
    if (in == null) throw EsTreeParseError(s"no `estree/$name` resource")
    val path = dir.resolve(name)
    try Files.copy(in, path, StandardCopyOption.REPLACE_EXISTING)
    finally in.close
    path.toFile.deleteOnExit()

  // JSON printer that never emits raw non-ASCII, so that the pipe stays safe
  // even for sources containing lone surrogates
  private val printer = Printer.noSpaces.copy(escapeNonAscii = true)

  private def request(req: Json): EsTree =
    val path = script.toString
    val builder = new ProcessBuilder(NODE, path)
    builder.redirectError(ProcessBuilder.Redirect.INHERIT)
    val process =
      try builder.start()
      catch
        case e: IOException =>
          throw EsTreeParseError(
            s"failed to launch `$NODE $path`: ${e.getMessage}",
          )
    try
      val line =
        try {
          val toScript =
            new OutputStreamWriter(process.getOutputStream, "UTF-8")
          toScript.write(printer.print(req))
          toScript.close()
          val fromScript = new BufferedReader(
            new InputStreamReader(process.getInputStream, "UTF-8"),
          )
          fromScript.readLine()
        } catch {
          case e: IOException => throw EsTreeParseError(e.getMessage)
        }
      if (line == null)
        throw EsTreeParseError("the parsing script wrote no response")
      val res = io.circe.parser.parse(line) match
        case Right(json) => json
        case Left(e) =>
          throw EsTreeParseError(s"invalid response: ${e.getMessage}")
      val cursor = res.hcursor
      if (cursor.get[Boolean]("ok").getOrElse(false))
        EsTree(cursor.downField("ast").focus.getOrElse {
          throw EsTreeParseError("the response has no AST")
        })
      else
        throw EsTreeParseError(
          cursor.get[String]("error").getOrElse("unknown error"),
        )
    finally process.destroy()
}
