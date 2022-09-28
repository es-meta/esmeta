package esmeta.util

trait StringNumeral {
  protected def getString(
    number: Int,
    digits: List[(Int, String)],
    lower: Boolean,
  ): String =
    val app = new Appender
    digits.foldLeft(number) {
      case (number, (from, to)) =>
        val str = if (lower) to.toLowerCase else to
        app >> str * (number / from)
        number % from
    }
    app.toString

  protected def getString(
    number: Int,
    base: Int,
    conversion: Int => String,
    lower: Boolean,
  ): String =
    var cur = number - 1
    var len = 1
    var mul = base
    var str = ""
    while (cur >= mul) { cur -= mul; mul *= base; len += 1 }
    for (_ <- Range(0, len))
      str = conversion(cur % base) + str
      cur /= base
    str
}
object RomanNumeral extends StringNumeral {
  def apply(number: Int, lower: Boolean = false): String = getString(
    number,
    List(
      1000 -> "M",
      900 -> "CM",
      500 -> "D",
      400 -> "CD",
      100 -> "C",
      90 -> "XC",
      50 -> "L",
      40 -> "XL",
      10 -> "X",
      9 -> "IX",
      5 -> "V",
      4 -> "IV",
      1 -> "I",
    ),
    lower,
  )
}
object AlphabetNumeral extends StringNumeral {
  def apply(number: Int, lower: Boolean = false): String =
    getString(number, 26, x => ('a' + x).toChar.toString, lower)
}
