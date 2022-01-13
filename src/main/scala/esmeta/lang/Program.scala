package esmeta.lang

/** programs for abstract algorithms */
case class Program(block: Block)

/** TODO algorithm blocks */
enum Block:
  case Order(steps: List[Step])
  case Unorder(steps: List[Step])
  case Figure(html: String)
object Block extends Parser[Block]

/** TODO algorithm steps */
enum Step:
  case Yet(str: String, block: Option[Block])
object Step extends Parser[Step]
