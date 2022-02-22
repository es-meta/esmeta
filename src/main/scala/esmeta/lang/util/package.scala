package esmeta.lang.util

import esmeta.util.BaseUtils.*

/** extensions for integers */
extension (int: Int) {
  def toOrdinal: String = int match
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case 4 => "fourth"
    case 5 => "fifth"
    case 6 => "sixth"
    case 7 => "seventh"
    case 8 => "eighth"
    case 9 => "ninth"
    case n =>
      n.toString + (n % 10 match
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => "th"
      )
}

/** extensions for integers */
extension (str: String) {
  def toIntFromOrdinal: Option[Int] = optional(str match
    case "first"   => 1
    case "second"  => 2
    case "third"   => 3
    case "fourth"  => 4
    case "fifth"   => 5
    case "sixth"   => 6
    case "seventh" => 7
    case "eighth"  => 8
    case "ninth"   => 9
    case _         => str.dropRight(2).toInt,
  )
}
