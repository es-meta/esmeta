package esmeta.lang

/** programs for abstract algorithms */
case class Program(block: Block) extends LangElem

/** TODO algorithm blocks */
enum Block extends LangElem:
  case Order(steps: List[Step])
  case Unorder(steps: List[Step])
  case Figure(lines: List[String])
object Block extends Parser[Block]

/** TODO algorithm steps */
enum Step extends LangElem:
  case Yet(str: String, block: Option[Block])
object Step extends Parser[Step]
