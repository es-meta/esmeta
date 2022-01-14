package esmeta.lang

/** a collector for metalanguage */
object StepCollector {

  /** get yet steps from a metalanguage element */
  def apply(elem: LangElem): List[Step] =
    val collector = new Collector
    collector.walk(elem)
    collector.steps.reverse

  // internal collector
  private class Collector extends UnitWalker {
    var steps: List[Step] = Nil
    override def walk(step: Step): Unit =
      steps ::= step
      super.walk(step)
  }
}
