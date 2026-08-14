package esmeta

@main
def findWhole(): Unit = {
  val cfg = CmdBuildCFG(List("-silent"))

  class FindMath extends ir.util.UnitWalker {
    var whole = 0
    var non = 0
    override def walk(lit: ir.LiteralExpr): Unit = lit match
      case ir.EMath(n) =>
        // println(s"Found EMath: $n")
        val strictlyWhole = n.scale == 0
        if (n.isWhole) {
          whole += 1
          if (!strictlyWhole) { println(s"Non-strictly whole number: $n") }
        }
        else {  
          non += 1
          println(s"Non-whole number: $n")
        }
      case _ =>
  }

  val finder = new FindMath
  finder.walk(cfg.program)
  println(s"Whole numbers: ${finder.whole}")
  println(s"Non-whole numbers: ${finder.non}")

}