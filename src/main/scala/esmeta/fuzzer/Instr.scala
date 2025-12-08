package esmeta.fuzzer

case class Instr(
  covered: Boolean = true,
  isFlipped: Boolean,
  trial: Int,
  // mutationEvent: (mutatorName, orig, mutated)
  mutationEvent: Option[(String, String, String)],
  // progress: (iter, elapsedTime)
  progress: Option[(Int, String)],
)
