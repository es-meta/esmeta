package esmeta.ir

/** coverage test for the corpus of the IR elements */
class CorpusTinyTest extends IRTest with ElemCoverage {
  val name: String = "irCorpusTest"

  // registration
  def init: Unit = checkElemCoverage("corpus")(IRTest.allElems.map(_._2))

  init
}
