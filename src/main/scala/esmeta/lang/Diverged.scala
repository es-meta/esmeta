package esmeta.lang

/** syntax element with diverged representation */
trait Diverged { this: Syntax =>
  var map: Map[String, Int] = Map()
}
