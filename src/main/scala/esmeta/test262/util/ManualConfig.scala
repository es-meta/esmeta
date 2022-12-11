package esmeta.test262.util

import esmeta.MANUALS_DIR
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.File

case class ManualConfig(
  filtered: Map[String, List[String]],
  supportedFeatures: List[String],
)
object ManualConfig {
  def apply(paths: List[String]): ManualConfig =
    val fileMap: Map[String, String] = (for {
      path <- paths
      file <- walkTree(s"$MANUALS_DIR/$path/test262")
      if jsonFilter(file.getName)
    } yield file.getName -> file.toString).toMap
    val filtered = fileMap
      .get("filtered.json")
      .fold(Map())(readJson[Map[String, List[String]]])
    val supportedFeatures = fileMap
      .get("supported-features.json")
      .fold(Nil)(readJson[List[String]])
    ManualConfig(filtered, supportedFeatures)
}
