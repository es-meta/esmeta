package esmeta.lang

/** TODO algorithm steps */
enum Step extends LangElem:
  case Block(steps: List[Step])
object Step extends Parser[Step]
