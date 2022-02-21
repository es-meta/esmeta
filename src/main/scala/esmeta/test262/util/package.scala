package esmeta.test262.util

import esmeta.DEBUG
import esmeta.test262.*

/** ordering of metadata */
given Ordering[MetaData] = Ordering.by(_.name)

/** extensions for list of metadata */
extension (data: List[MetaData]) {

  /** remove metadata using a filter */
  def remove(pairs: (String, MetaData => Boolean)*): List[MetaData] =
    pairs.foldLeft(data) {
      case (data, (desc, f)) =>
        val (filtered, removed) = data.foldLeft(List[MetaData](), 0) {
          case ((l, count), meta) =>
            if (f(meta)) (l, count + 1)
            else (meta :: l, count)
        }
        if (DEBUG)
          println(f"[DEBUG] - $desc%-30s: $removed%,5d tests are removed")
        filtered.reverse
    }

  /** get the summary */
  def summary: ConfigSummary = {
    val (normalL, errorL) = data.partition(_.negative.isEmpty)
    ConfigSummary(
      data,
      normalL.map {
        case MetaData(name, _, _, i, _, _, _) =>
          NormalConfig(name, i)
      },
      errorL.collect {
        case MetaData(name, n, _, i, _, _, _) =>
          ErrorConfig(name, n.get, i)
      },
    )
  }
}

// TODO
// object TestConfigJsonProtocol {
//   implicit val normalTestConfigDecoder: Decoder[NormalTestConfig] =
//     deriveDecoder
//   implicit val normalTestConfigEncoder: Encoder[NormalTestConfig] =
//     deriveEncoder
//   implicit val errorTestConfigDecoder: Decoder[ErrorTestConfig] = deriveDecoder
//   implicit val errorTestConfigEncoder: Encoder[ErrorTestConfig] = deriveEncoder
//   implicit val test262ConfigDecoder: Decoder[Test262Config] = deriveDecoder
//   implicit val test262ConfigEncoder: Encoder[Test262Config] = deriveEncoder
//   implicit val test262ConfigSummaryDecoder: Decoder[Test262ConfigSummary] =
//     deriveDecoder
//   implicit val test262ConfigSummaryEncoder: Encoder[Test262ConfigSummary] =
//     deriveEncoder
// }
