package esmeta.util

/** helper for time */
case class Time(millis: Long = 0L) {
  def seconds: Double = millis / 1000.0
  def minutes: Double = seconds / 60.0
  def hours: Double = minutes / 60.0
  override def toString: String = f"$millis%,d ms [$simpleString]"
  def simpleString: String =
    val s = millis / 1000
    val m = s / 60
    val h = m / 60
    val d = h / 24
    var str = ""
    if (d > 0) str += f"$d days "
    if (h > 0) str += f"${h % 24}%02d:"
    str += f"${m % 60}%02d:${s % 60}%02d"
    str
}
