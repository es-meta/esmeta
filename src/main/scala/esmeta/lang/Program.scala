package esmeta.lang

/** programs for abstract algorithms */
case class Program(block: Block) extends LangElem

// -----------------------------------------------------------------------------
// algorithm blocks
// -----------------------------------------------------------------------------
sealed trait Block extends LangElem
object Block extends Parser[Block]
case class Order(steps: List[Step]) extends Block
case class Unorder(steps: List[Step]) extends Block
case class Figure(lines: List[String]) extends Block

// -----------------------------------------------------------------------------
// TODO algorithm steps
// -----------------------------------------------------------------------------
sealed trait Step extends LangElem
object Step extends Parser[Step]
case class Yet(str: String, block: Option[Block]) extends Step
