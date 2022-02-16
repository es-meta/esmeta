package esmeta.js.util

import esmeta.js.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.input.Position
import scala.util.matching.Regex

/** JavaScript special unicodes */
trait UnicodeParsers extends BasicParsers with EPackratParsers {
  val ZWNJ = "\u200C".r
  val ZWJ = "\u200D".r
  val ZWNBSP = "\uFEFF".r
  // white spaces
  val TAB = "\u0009".r
  val VT = "\u000B".r
  val FF = "\u000C".r
  val SP = "\u0020".r
  val NBSP = "\u00A0".r
  // TODO automatically extract category "Zs"
  val USP =
    "[\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]".r
  // line terminators
  val LF = "\u000A".r
  val CR = "\u000D".r
  val LS = "\u2028".r
  val PS = "\u2029".r

  lazy val lines = "[\u000A\u000D\u2028\u2029]".r
  lazy val Unicode = "(?s).".r
  lazy val IDStart =
    Unicode.filter(s => CodePoint.IDStart contains toCodePoint(s))
  lazy val IDContinue =
    Unicode.filter(s => CodePoint.IDContinue contains toCodePoint(s))

  protected def toCodePoint(str: String): Int =
    def check4B(i: Int): Boolean =
      str.codePointCount(i, str.length min (i + 2)) == 1
    def aux(i: Int, acc: Int): Int =
      if (i >= str.length) acc
      else
        val nextAcc = str.codePointAt(i) + (acc * (1 << 16))
        aux(if (check4B(i)) i + 2 else i + 1, nextAcc)
    aux(0, 0)

  // abbreviated code points mapping
  val abbrCPs: Map[String, Regex] = Map(
    "ZWNJ" -> ZWNJ,
    "ZWJ" -> ZWJ,
    "ZWNBSP" -> ZWNBSP,
    "TAB" -> TAB,
    "VT" -> VT,
    "FF" -> FF,
    "SP" -> SP,
    "NBSP" -> NBSP,
    "USP" -> USP,
    "LF" -> LF,
    "CR" -> CR,
    "LS" -> LS,
    "PS" -> PS,
  )
}
