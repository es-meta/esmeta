package esmeta.util

import esmeta.MANUALS_DIR
import esmeta.spec.Spec
import esmeta.test262.util.ManualConfig
import esmeta.ty.TyModel
import esmeta.ty.util.JsonProtocol.given
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.File

/** manual information helpers */
case class ManualInfo(version: Option[Spec.Version]) {
  import ManualInfo.*

  /** get algorithm files */
  def algoFiles: List[File] = getAlgoFiles(paths)

  /** get IR function files */
  def funcFiles: List[File] = getFuncFiles(paths)

  /** get compile rules */
  def compileRule: CompileRule = getCompileRule(paths)

  /** get type model */
  def tyModel: TyModel = getTyModel(paths)

  /** get bugfixes */
  def bugfixFile: Option[File] = bugfixPath.map(File(_))
  def bugfixPath: Option[String] = getPath("bugfix.patch")

  /** get test262 */
  def test262: ManualConfig = ManualConfig(paths)

  /** get tycheck-ignore.json */
  def tycheckIgnore: Option[String] = getPath("tycheck-ignore.json")

  private def getPath(name: String): Option[String] =
    version.fold(None)(version => {
      val path = s"$MANUALS_DIR/${version.shortHash}/$name"
      if (exists(path)) Some(path) else None
    })
  private lazy val paths: List[String] =
    List("default") ++ version.map(_.shortHash)
}
object ManualInfo {
  type CompileRule = Map[String, Map[String, String]]

  val defaultPaths: List[String] = List("default")

  /** get algorithm files */
  def getAlgoFiles(paths: List[String] = defaultPaths): List[File] =
    getFiles(paths, algoFilter)

  /** get IR function files */
  def getFuncFiles(paths: List[String] = defaultPaths): List[File] =
    getFiles(paths, irFilter)

  /** get files */
  def getFiles(
    paths: List[String] = defaultPaths,
    filter: String => Boolean = _ => true,
  ): List[File] = for {
    path <- paths
    file <- walkTree(s"$MANUALS_DIR/$path")
    if filter(file.getName)
  } yield file

  /** get compile rules */
  def getCompileRule(paths: List[String] = defaultPaths): CompileRule = paths
    .map(path => s"$MANUALS_DIR/$path/rule.json")
    .map(path => optional(readJson[CompileRule](path)).getOrElse(Map()))
    .foldLeft[CompileRule](Map())(_ ++ _)

  /** get type model */
  def getTyModel(paths: List[String] = defaultPaths): TyModel = paths
    .map(path => s"$MANUALS_DIR/$path/ty-model.json")
    .map(path => optional(readJson[TyModel](path)).getOrElse(TyModel()))
    .foldLeft[TyModel](TyModel())(_ ++ _)
}
