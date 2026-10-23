package esmeta.lang

/** coverage test for the corpus of the metalanguage syntax */
class CorpusTinyTest extends LangTest with SyntaxCoverage {
  val name: String = "langCorpusTest"

  // registration
  def init: Unit = checkSyntaxCoverage("corpus")(LangTest.allSyntax.map(_._2))

  init
}
