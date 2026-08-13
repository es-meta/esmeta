package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.util.SystemUtils.*
import io.circe.*

/** visualizer section-to-function mapping test */
class DumpSecIdToFuncInfoSmallTest extends ESMetaTest {
  val name: String = "dumpSecIdToFuncInfoTest"
  def category: String = "dump"

  private lazy val cfg = ESMetaTest.cfg
  private lazy val result =
    DumpSecIdToFuncInfo(cfg)
    readJson[Map[String, (Int, String, List[Int])]](
      s"$DUMP_VISUALIZER_LOG_DIR/secIdToFunc.json",
    )

  private def candidateIds(info: (Int, String, List[Int])): List[Int] =
    info._1 :: info._3

  private def checkTemplate(
    sectionId: String,
    templateName: String,
  ): Unit =
    val info = result(sectionId)
    val ids = candidateIds(info)
    val names = ids.map(cfg.funcMap(_).name.stripPrefix("INTRINSICS.")).toSet
    val expected = cfg.intrinsics.getInstances(templateName).keySet
    assert(info._2 == s"_${templateName}_")
    assert(names == expected)
    assert(ids.size == expected.size)

  // registration
  def init: Unit =
    check("preserve generic names and all template instances") {
      checkTemplate("sec-nativeerror", "NativeError")
      checkTemplate("sec-typedarray", "TypedArray")
    }

  init
}
