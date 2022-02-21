package esmeta.test262

/** test configuration summary */
case class ConfigSummary(
  data: List[MetaData],
  normal: List[NormalConfig],
  error: List[ErrorConfig],
) extends Test262Elem

/** configuration for normal tests */
case class NormalConfig(
  name: String,
  includes: List[String],
) extends Test262Elem

/** configuration for error tests */
case class ErrorConfig(
  name: String,
  errorName: String,
  includes: List[String],
) extends Test262Elem
