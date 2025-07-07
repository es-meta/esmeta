package esmeta.fuzzer

/** the information for fuzzer instrumentation */
case class InstInfo(
  iter: Int,
  cur: String,
  prev: String,
  mutatorName: String,
)
