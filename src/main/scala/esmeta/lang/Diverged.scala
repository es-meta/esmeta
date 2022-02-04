package esmeta.lang

/** syntax element with diverged representation */
trait Diverged { this: Syntax =>
  private var map: Map[String, Int] = Map()
  def setMap(m: Map[String, Int]): this.type = { map = m; this }
}
