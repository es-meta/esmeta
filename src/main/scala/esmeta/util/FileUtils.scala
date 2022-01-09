package esmeta.util

import java.io.{Reader, File, PrintWriter}
import java.nio.file.{Files, StandardCopyOption}
import esmeta.*
import esmeta.error.*
import esmeta.util.BaseUtils.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.io.Source
import scala.sys.process.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*

/** file utilities */
object FileUtils {
  // encoding
  private val ENC = "utf8"

  /** file reader */
  def fileReader(filename: String): Reader =
    Source.fromFile(filename, ENC).bufferedReader

  /** file trees with filename */
  def walkTree(filename: String): Iterable[File] = walkTree(new File(filename))

  /** file trees with files */
  def walkTree(file: File): Iterable[File] =
    Seq(file) ++ new Iterable[File] {
      def iterator: Iterator[File] =
        if (file.isDirectory) file.listFiles.iterator
        else Iterator.empty
    }.flatMap(walkTree)

  /** extension filter */
  def extFilter(ext: String): String => Boolean = _.endsWith(s".$ext")

  /** print writer */
  def getPrintWriter(filename: String): PrintWriter =
    new PrintWriter(new File(filename))

  /** dump given data to a file */
  def dumpFile(data: Any, filename: String): Unit =
    val nf = getPrintWriter(filename)
    nf.print(data)
    nf.close()

  /** dump given data to a file and show message */
  def dumpFile(name: String, data: Any, filename: String): Unit =
    val res = dumpFile(data, filename)
    println(s"dumped $name to $filename.")

  /** dump given data in a JSON format */
  def dumpJson[T](
    data: T,
    filename: String,
    noSpace: Boolean = false,
  )(using encoder: Encoder[T]): Unit =
    val json = data.asJson
    dumpFile(if (noSpace) json.noSpaces else json.spaces2, filename)

  /** dump given data in a JSON format and show message */
  def dumpJson[T](
    name: String,
    data: T,
    filename: String,
    noSpace: Boolean,
  )(using encoder: Encoder[T]): Unit =
    dumpJson(data, filename, noSpace)
    println(s"dumped $name to $filename in a JSON format.")

  /** get first filename */
  def getFirstFilename(globalConfig: GlobalConfig, msg: String): String =
    globalConfig.args.headOption.getOrElse(throw NoFileError(msg))

  /** read file */
  def readFile(filename: String): String =
    val source = Source.fromFile(filename, ENC)
    val str = source.mkString
    source.close
    str

  /** read JSON */
  def readJson[T](filename: String)(implicit decoder: Decoder[T]): T =
    parse(readFile(filename)) match {
      case Left(err) => throw err
      case Right(json) =>
        json.as[T] match {
          case Left(err) => throw err
          case Right(v)  => v
        }
    }

  /** read HTML */
  def readHtml(filename: String): org.jsoup.nodes.Document =
    HtmlUtils.parseHtml(readFile(filename))

  /** delete files */
  def deleteFile(filename: String): Unit = new File(filename).delete

  /** change extension */
  def changeExt(from: String, to: String): String => String =
    filename => filename.substring(0, filename.length - from.length) + to

  /** get name without extension */
  def removedExt(filename: String): String =
    filename.split('.').dropRight(1).mkString(".")

  /** get extension */
  def getExt(filename: String): String =
    filename.split('.').last

  /** renamed filename */
  def renameFile(from: String, to: String): Unit =
    new File(from).renameTo(new File(to))

  /** copy file */
  def copyFile(from: String, to: String): Unit = Files.copy(
    new File(from).toPath,
    new File(to).toPath,
    StandardCopyOption.REPLACE_EXISTING,
  )

  /** create directories */
  def mkdir(name: String): Unit = new File(name).mkdirs

  /** clean directories */
  def cleanDir(name: String) = for (file <- walkTree(name)) file.delete

  /** remove directories */
  def rmdir(name: String): Unit = {
    def deleteRecursively(f: File): Boolean =
      if (f.isDirectory) f.listFiles match {
        case files: Array[File] => files.foreach(deleteRecursively)
        case null               =>
      }
      f.delete()
    deleteRecursively(new File(name))
  }

  /** file existence check */
  def exists(name: String): Boolean = new File(name).exists

  /** check whether a shell command is normally terminated */
  def isNormalExit(str: String): Boolean = optional(executeCmd(str)).isDefined

  /** execute shell command with given dir, default to CUR_DIR */
  def executeCmd(cmdStr: String, dir: String = CUR_DIR): String =
    var cmd = s"$cmdStr 2> /dev/null"
    var directory = new File(dir)
    var process = Process(Seq("sh", "-c", cmd), directory)
    process.!!

  /** change git version */
  def changeVersion(target: String, dir: String = CUR_DIR): Unit =
    executeCmd(s"git checkout $target", dir)

  /** get git current version */
  def currentVersion(dir: String = CUR_DIR): String =
    executeCmd(s"git rev-parse HEAD", dir).trim

  /** set timeout with optinal limitation */
  def timeout[T](f: => T, limit: Option[Long]): T =
    timeout(f, limit.fold[Duration](Duration.Inf)(_.seconds))

  /** set timeout with limitation */
  def timeout[T](f: => T, limit: Long): T = timeout(f, limit.seconds)

  /** set timeout with duration */
  def timeout[T](f: => T, duration: Duration): T =
    Await.result(Future(f), duration)
}
