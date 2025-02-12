package esmeta.parser

import esmeta.error.*
import esmeta.es.*
import esmeta.state.{BigInt => BigIntV, *}
import esmeta.util.BaseUtils.*
import scala.annotation.switch
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.*

object ESValueParser extends UnicodeParsers with RegexParsers {
  // empty data for EPackratParser
  case class Data() extends DataType { def next = this }
  def defaultData = Data()

  // not skip white spaces
  override def skipWhitespace = false

  // Math parsers
  type M = Parser[Math]

  // Number parsers
  type N = Parser[Number]

  // String parsers
  type S = Parser[String]
  extension (x0: S) {
    inline def rep: S = x0.* ^^ { _.mkString }
    inline def rep1: S = x0.+ ^^ { _.mkString }
    inline def opt: S = x0.? ^^ { _.getOrElse("") }
    inline def %(x1: => S): S = x0 ~ x1 ^^ { case x ~ y => x + y }
    inline def exc: S = Predef.SourceCharacter.filter(parseAll(x0, _).isEmpty)
  }

  def str2number(str: String): Number = optional {
    StringNumericValue.of("StringNumericLiteral")(str)
  }.getOrElse(Number(Double.NaN))
  def str2bigint(str: String): BigIntV | Undef = optional {
    MV.of("StringIntegerLiteral")(str).toBigInt
  }.getOrElse(Undef)

  // predefined parsers
  object Predef {
    lazy val lf = toParser(LF)
    lazy val cr = toParser(CR)
    lazy val ls = toParser(LS)
    lazy val ps = toParser(PS)
    lazy val SourceCharacter: S = "(?s).".r
    lazy val WhiteSpace: S = toParser(WhiteSpaceCPs)
    lazy val LineTerminator: S = toParser(LineTerminatorCPs)
    lazy val LineTerminatorSequence: S = lf | cr <~ not(lf) | ls | ps | cr % lf
    lazy val LineContinuation: S = "\\" % LineTerminatorSequence
    lazy val StrWhiteSpaceChar: S = toParser(StrWhiteSpaceCharCPs)
    lazy val StrWhiteSpace: S = StrWhiteSpaceChar.rep1
    lazy val SignedInteger: S = "[+-]?[0-9]+".r
    lazy val BinaryIntegerLiteral: S = "0[bB][01]+".r
    lazy val OctalIntegerLiteral: S = "0[oO][0-7]+".r
    lazy val HexIntegerLiteral: S = "0[xX][0-9a-fA-F]+".r
    lazy val NonDecimalIntegerLiteral: S =
      BinaryIntegerLiteral | OctalIntegerLiteral | HexIntegerLiteral
    lazy val StrIntegerLiteral: S = NonDecimalIntegerLiteral | SignedInteger
    lazy val LegacyOctalEscapeSequence: S =
      "0" <~ guard("8" | "9") |
      NonZeroOctalDigit <~ not(OctalDigit) |
      ZeroToThree % OctalDigit <~ not(OctalDigit) |
      FourToSeven % OctalDigit |
      ZeroToThree % OctalDigit % OctalDigit
    lazy val NonZeroOctalDigit: S = "[1-7]".r
    lazy val OctalDigit: S = "[0-7]".r
    lazy val ZeroToThree: S = "[0-3]".r
    lazy val FourToSeven: S = "[4-7]".r
    lazy val NonOctalDecimalEscapeSequence: S = "[89]".r
    lazy val SingleEscapeCharacter: S = "[btnvfrn'\"\\\\]".r
    lazy val DecimalDigit: S = "[0-9]".r
    lazy val EscapeCharacter: S =
      SingleEscapeCharacter | DecimalDigit | "x" | "u"
    lazy val HexDigit: S = "[0-9a-fA-F]".r
    lazy val HexDigits: S = "[0-9a-fA-F]+".r
    lazy val NotEscapeSequence: S =
      "0" % DecimalDigit |
      DecimalDigit.filter(_ != "0") |
      "x" <~ not(HexDigit) |
      "x" % HexDigit <~ not(HexDigit) |
      "u" % HexDigit <~ not(HexDigit) |
      "u" % HexDigit % HexDigit <~ not(HexDigit) |
      "u" % HexDigit % HexDigit % HexDigit <~ not(HexDigit) |
      "u" <~ not(HexDigit) <~ not("{") |
      "u{" <~ not(HexDigit) |
      "u{" % NotCodePoint <~ not(HexDigit) |
      "u{" % CodePoint <~ not(HexDigit) <~ not("}")
    lazy val CodePoint: S =
      HexDigits.filter { s => Math.fromHex(s).toInt <= 0x10ffff }
    lazy val NotCodePoint: S =
      HexDigits.filter { s => Math.fromHex(s).toInt > 0x10ffff }
  }

  // ---------------------------------------------------------------------------
  // StringValue of |IdentifierName| and |PrivateIdentifier|
  // (https://tc39.es/ecma262/#sec-numericvalue)
  // ---------------------------------------------------------------------------
  object StringValue {
    val of: Map[String, String => Str] = from("StringValue")(
      "IdentifierName \\ (ReservedWord)" -> (IdentifierName ^^ { Str(_) }),
      "IdentifierName" -> (IdentifierName ^^ { Str(_) }),
      "PrivateIdentifier" -> (PrivateIdentifier ^^ { Str(_) }),
    )

    lazy val IdentifierName: S =
      IdentifierStart % IdentifierPart.rep
    lazy val IdentifierStart: S =
      IDStart |
      "$" |
      "_" |
      "\\" ~> SV.UnicodeEscapeSequence
    lazy val IdentifierPart: S =
      IDContinue |
      "$" |
      "\\" ~> SV.UnicodeEscapeSequence
    lazy val PrivateIdentifier: S = "#" % IdentifierName
  }

  // ---------------------------------------------------------------------------
  // Numeric Value of |NumericLiteral|
  // (https://tc39.es/ecma262/#sec-numericvalue)
  // ---------------------------------------------------------------------------
  object NumericValue {
    val of: Map[String, String => Numeric] = Map(
      "NumericLiteral" -> { s =>
        (s.take(2): @switch) match
          case "0b" | "0B" => toNumeric(s.drop(2), 2)
          case "0o" | "0O" => toNumeric(s.drop(2), 8)
          case "0x" | "0X" => toNumeric(s.drop(2), 16)
          case _ =>
            if (isOctalString(s)) toNumeric(s, 8)
            else toNumeric(s, 10)
      },
    )

    // convert to a numeric value with a base
    private def toNumeric(s: String, base: Int): Numeric =
      val ss = noSep(s)
      if (ss.endsWith("n")) BigIntV(BigInt(ss.dropRight(1), base))
      else if (base == 10) Number(Math(ss).toDouble)
      else Number(Math.from(ss, base).toDouble)
  }

  // ---------------------------------------------------------------------------
  // MV
  // ---------------------------------------------------------------------------
  object MV {
    val of = Map[String, String => Math](
      // https://tc39.es/ecma262/#sec-static-semantics-mv
      "NumericLiteral" -> { s =>
        val ss = noSuffix(s)
        handleNonDecimal(ss).getOrElse(handleLegacyOctal(ss))
      },
      "DecimalBigIntegerLiteral" -> { s => Math(noSuffix(s)) },
      "NonDecimalIntegerLiteral" -> { handleNonDecimal(_).get },
      "DecimalLiteral" -> { s => Math(s) },
      "DecimalIntegerLiteral" -> { s => Math(s) },
      "DecimalDigits" -> { s => Math(s) },
      "DecimalDigit" -> { s => Math(s) },
      "NonZeroDigit" -> { s => Math(s) },
      "ExponentPart" -> { s => Math(s.tail) },
      "SignedInteger" -> { s => Math(s) },
      "BinaryIntegerLiteral" -> { s => Math.fromBinary(s.drop(2)) },
      "BinaryDigits" -> { s => Math.fromBinary(noSep(s)) },
      "BinaryDigit" -> { s => Math(s) },
      "OctalIntegerLiteral" -> { s => Math.fromOctal(s.drop(2)) },
      "OctalDigits" -> { s => Math.fromOctal(noSep(s)) },
      "LegacyOctalIntegerLiteral" -> { s => Math.fromOctal(s) },
      "NonOctalDecimalIntegerLiteral" -> { s => Math(s) },
      "LegacyOctalLikeDecimalIntegerLiteral" -> { s => Math(s) },
      "OctalDigit" -> { s => Math(s) },
      "HexIntegerLiteral" -> { s => Math.fromHex(s.drop(2)) },
      "HexDigits" -> { s => Math.fromHex(noSep(s)) },
      "HexDigit" -> { s => Math.fromHex(s) },

      // https://tc39.es/ecma262/#sec-string-literals-static-semantics-mv
      "LegacyOctalEscapeSequence" -> { s => Math.fromOctal(s) },
      "NonZeroOctalDigit" -> { s => Math(s) },
      "ZeroToThree" -> { s => Math(s) },
      "FourToSeven" -> { s => Math(s) },
      "NonOctalDecimalEscapeSequence" -> { s => Math(s) },
      "HexEscapeSequence" -> { s => Math.fromHex(s.tail) },
      "Hex4Digits" -> { s => Math.fromHex(s) },
    ) ++ from("MV")(
      // https://tc39.es/ecma262/#sec-runtime-semantics-mv-for-stringintegerliteral
      "StringIntegerLiteral" -> StringIntegerLiteral,
    )

    lazy val HexDigit: M =
      "[0-9a-fA-F]".r ^^ { case s => Math.fromHex(s) }
    lazy val HexDigits: M =
      "[0-9a-fA-F]+".r ^^ { case s => Math.fromHex(noSep(s)) }
    lazy val Hex4Digits: M =
      "[0-9a-fA-F]{4}".r ^^ { case s => Math.fromHex(s) }
    lazy val StringIntegerLiteral: M =
      import Predef.{StrWhiteSpace, StrIntegerLiteral}
      opt(StrWhiteSpace) ~> {
        StrIntegerLiteral ^^ { s => handleNonDecimal(s).getOrElse(Math(s)) }
      } <~ opt(StrWhiteSpace) |
      opt(StrWhiteSpace) ^^^ Math(0)
    lazy val CodePoint: M = MV.HexDigits.filter { _.toInt <= 0x10ffff }
    lazy val NotCodePoint: M = MV.HexDigits.filter { _.toInt > 0x10ffff }
    lazy val NonDecimalIntegerLiteral: M =
      Predef.BinaryIntegerLiteral ^^ { s => Math.fromBinary(s.drop(2)) } |
      Predef.OctalIntegerLiteral ^^ { s => Math.fromOctal(s.drop(2)) } |
      Predef.HexIntegerLiteral ^^ { s => Math.fromHex(s.drop(2)) }

    // remove suffix "n" from a string
    private inline def noSuffix(s: String): String =
      if (s.endsWith("n")) s.dropRight(1) else s

    // handle non-decimal integer literals
    private inline def handleNonDecimal(s: String): Option[Math] =
      (s.take(2): @switch) match
        case "0b" | "0B" => Some(Math.fromBinary(noSuffix(s.drop(2))))
        case "0o" | "0O" => Some(Math.fromOctal(noSuffix(s.drop(2))))
        case "0x" | "0X" => Some(Math.fromHex(noSuffix(s.drop(2))))
        case _           => None

    // handle legacy octal integer literals
    private inline def handleLegacyOctal(s: String): Math =
      if (isOctalString(s)) Math.fromOctal(s) else Math(s)
  }

  // ---------------------------------------------------------------------------
  // StringNumericValue of |StringNumericLiteral|
  // (https://tc39.es/ecma262/#sec-runtime-semantics-stringnumericvalue)
  // ---------------------------------------------------------------------------
  object StringNumericValue {
    val of: Map[String, String => Number] = from("StringNumericValue")(
      "StringNumericLiteral" -> StringNumericLiteral,
      "StrNumericLiteral" -> StrNumericLiteral,
      "StrDecimalLiteral" -> StrDecimalLiteral,
      "StrUnsignedDecimalLiteral" -> StrUnsignedDecimalLiteral,
    )

    lazy val StringNumericLiteral: N =
      import Predef.StrWhiteSpace
      opt(StrWhiteSpace) ~> {
        StringNumericValue.StrNumericLiteral
      } <~ opt(StrWhiteSpace) |
      opt(Predef.StrWhiteSpace) ^^^ Number(0)
    lazy val StrNumericLiteral: N =
      MV.NonDecimalIntegerLiteral ^^ { n => Number(n.toDouble) } |
      StringNumericValue.StrDecimalLiteral
    lazy val StrDecimalLiteral: N =
      "[-+]?".r ~ StringNumericValue.StrUnsignedDecimalLiteral ^^ {
        case s ~ Number(n) => if (s == "-") Number(-n) else Number(n)
      }
    lazy val StrUnsignedDecimalLiteral: N =
      "Infinity" ^^^ Number(Double.PositiveInfinity) |
      "[-+.eE0-9]+".r ^^ { s => Number(Math(s).toDouble) }
  }

  // ---------------------------------------------------------------------------
  // SV of |StringLiteral|
  // (https://tc39.es/ecma262/#sec-static-semantics-sv)
  // ---------------------------------------------------------------------------
  object SV {
    val of: Map[String, String => Str] = from("SV")(
      "StringLiteral" -> (StringLiteral ^^ { Str(_) }),
    )

    lazy val StringLiteral: S =
      "\"" ~> SV.DoubleStringCharacters.opt <~ "\"" |
      "'" ~> SV.SingleStringCharacters.opt <~ "'"
    lazy val DoubleStringCharacters: S = SV.DoubleStringCharacter.rep1
    lazy val SingleStringCharacters: S = SV.SingleStringCharacter.rep1
    lazy val DoubleStringCharacter: S =
      ("\"" | "\\" | Predef.LineTerminator).exc |
      Predef.ls |
      Predef.ps |
      "\\" ~> SV.EscapeSequence |
      Predef.LineContinuation ^^^ ""
    lazy val SingleStringCharacter: S =
      ("'" | "\\" | Predef.LineTerminator).exc |
      Predef.ls |
      Predef.ps |
      "\\" ~> SV.EscapeSequence |
      Predef.LineContinuation ^^^ ""
    lazy val EscapeSequence: S =
      SV.CharacterEscapeSequence |
      "0" ^^^ "\u0000" |
      Predef.LegacyOctalEscapeSequence ^^ { s => cp2str(Math.fromOctal(s)) } |
      Predef.NonOctalDecimalEscapeSequence |
      SV.HexEscapeSequence |
      SV.UnicodeEscapeSequence
    lazy val CharacterEscapeSequence: S =
      Predef.SingleEscapeCharacter ^^ {
        case "b" => "\u0008"
        case "t" => "\u0009"
        case "n" => "\u000a"
        case "v" => "\u000b"
        case "f" => "\u000c"
        case "r" => "\u000d"
        case s   => s
      } |
      SV.NonEscapeCharacter
    lazy val NonEscapeCharacter: S =
      (Predef.EscapeCharacter | Predef.LineTerminator).exc
    lazy val HexEscapeSequence: S =
      "x" ~> MV.HexDigit ~ MV.HexDigit ^^ {
        case x ~ y => Character.toChars(16 * x.toInt + y.toInt).mkString
      }
    lazy val Hex4Digits: S =
      MV.Hex4Digits ^^ { case x => Character.toChars(x.toInt).mkString }
    lazy val UnicodeEscapeSequence: S =
      "u" ~> SV.Hex4Digits |
      "u{" ~> MV.CodePoint <~ "}" ^^ { case n => cp2str(n) }
    lazy val TemplateEscapeSequence: S =
      SV.CharacterEscapeSequence |
      "0" <~ not(Predef.DecimalDigit) ^^^ "\u0000" |
      SV.HexEscapeSequence |
      SV.UnicodeEscapeSequence

    // convert a code point to a string
    private def cp2str(cp: Math): String = cp2str(cp.toInt)

    // convert a code point to a string
    private def cp2str(cp: Int): String = Character.toChars(cp).mkString
  }

  // ---------------------------------------------------------------------------
  // TV of |NoSubstitutionTemplate|, |TemplateHead|, |TemplateMiddle|, and
  // |TemplateTail|
  // (https://tc39.es/ecma262/#sec-static-semantics-tv)
  // ---------------------------------------------------------------------------
  object TV {
    val of: Map[String, String => (Str | Undef)] = from("TV")(
      "NoSubstitutionTemplate" -> NoSubstitutionTemplate,
      "TemplateHead" -> TemplateHead,
      "TemplateMiddle" -> TemplateMiddle,
      "TemplateTail" -> TemplateTail,
    ).map { case (x, p) => x -> ((s: String) => handleUndef(p(s))) }

    case object ReturnUndef extends Throwable
    lazy val NoSubstitutionTemplate: S =
      "`" ~> TV.TemplateCharacters.opt <~ "`"
    lazy val TemplateHead: S =
      "`" ~> TV.TemplateCharacters.opt <~ "${"
    lazy val TemplateMiddle: S =
      "}" ~> TV.TemplateCharacters.opt <~ "${"
    lazy val TemplateTail: S =
      "}" ~> TV.TemplateCharacters.opt <~ "`"
    lazy val TemplateCharacter: S =
      "$" <~ not("{") |
      "\\" ~> SV.TemplateEscapeSequence |
      "\\" ~> Predef.NotEscapeSequence ^^^ { throw ReturnUndef } |
      TV.LineContinuation |
      TRV.LineTerminatorSequence |
      ("`" | "\\" | "$" | Predef.LineTerminator).exc
    lazy val TemplateCharacters: S =
      TV.TemplateCharacter.rep1
    lazy val LineContinuation: S =
      "\\" ~ Predef.LineTerminatorSequence ^^^ ""

    private def handleUndef(str: => String): Str | Undef =
      try Str(str)
      catch case TV.ReturnUndef => Undef
  }

  // ---------------------------------------------------------------------------
  // TRV of |NoSubstitutionTemplate|, |TemplateHead|, |TemplateMiddle|, and
  // |TemplateTail|
  // (https://tc39.es/ecma262/#sec-static-semantics-trv)
  // ---------------------------------------------------------------------------
  object TRV {
    val of: Map[String, String => Str] = from("TRV")(
      "NoSubstitutionTemplate" -> NoSubstitutionTemplate,
      "TemplateHead" -> TemplateHead,
      "TemplateMiddle" -> TemplateMiddle,
      "TemplateTail" -> TemplateTail,
    ).map { case (x, p) => x -> ((s: String) => Str(p(s))) }
    lazy val NoSubstitutionTemplate: S =
      "`" ~> TRV.TemplateCharacters.opt <~ "`"
    lazy val TemplateHead: S =
      "`" ~> TRV.TemplateCharacters.opt <~ "${"
    lazy val TemplateMiddle: S =
      "}" ~> TRV.TemplateCharacters.opt <~ "${"
    lazy val TemplateTail: S =
      "}" ~> TRV.TemplateCharacters.opt <~ "`"
    lazy val TemplateCharacters: S = TRV.TemplateCharacter.rep1
    lazy val TemplateCharacter: S =
      "$" <~ not("{") |
      "\\" % TRV.TemplateEscapeSequence |
      "\\" % TRV.NotEscapeSequence |
      TRV.LineContinuation |
      TRV.LineTerminatorSequence |
      ("`" | "\\" | "$" | Predef.LineTerminator).exc
    lazy val TemplateEscapeSequence: S =
      TRV.CharacterEscapeSequence |
      "0" ^^^ "\u0030" |
      TRV.HexEscapeSequence |
      TRV.UnicodeEscapeSequence
    lazy val NotEscapeSequence: S =
      "0" % TRV.DecimalDigit |
      TRV.DecimalDigit.filter(_ != "0") |
      "x" <~ not(HexDigit) |
      "x" % TRV.HexDigit <~ not(Predef.HexDigit) |
      "u" <~ not(Predef.HexDigit) <~ not("{") |
      "u" % TRV.HexDigit <~ not(Predef.HexDigit) |
      "u" % TRV.HexDigit % TRV.HexDigit <~ not(Predef.HexDigit) |
      "u" % TRV.HexDigit % TRV.HexDigit % TRV.HexDigit <~ not(Predef.HexDigit) |
      "u{" <~ not(Predef.HexDigit) |
      "u{" % TRV.NotCodePoint <~ not(Predef.HexDigit) |
      "u{" % TRV.CodePoint <~ not(Predef.HexDigit) <~ not("}")
    lazy val DecimalDigit: S = "[0-9]".r
    lazy val CharacterEscapeSequence: S =
      SV.NonEscapeCharacter |
      TRV.SingleEscapeCharacter
    lazy val SingleEscapeCharacter: S = Predef.SingleEscapeCharacter
    lazy val HexEscapeSequence: S = "x" % TRV.HexDigit % TRV.HexDigit
    lazy val UnicodeEscapeSequence: S =
      "u" % TRV.Hex4Digits |
      "u{" % TRV.CodePoint % "}"
    lazy val Hex4Digits: S =
      TRV.HexDigit % TRV.HexDigit % TRV.HexDigit % TRV.HexDigit
    lazy val HexDigit: S = Predef.HexDigit
    lazy val LineContinuation: S = "\\" % LineTerminatorSequence
    lazy val LineTerminatorSequence: S =
      Predef.cr ~> Predef.lf |
      Predef.cr ^^^ "\u000a" |
      Predef.lf |
      Predef.ls |
      Predef.ps
    lazy val CodePoint: S = Predef.CodePoint
    lazy val NotCodePoint: S = Predef.NotCodePoint
  }

  private def from[T](sdoName: String)(
    pair: (String, Parser[T])*,
  ): Map[String, String => T] = (for {
    (name, parser) <- pair
  } yield name -> (s => get(s"$sdoName.$name", parser, s))).toMap

  private def get[T](name: String, rule: Parser[T], str: String): T =
    parseAll(rule, str) match {
      case Success(res, _) => res
      case f => throw ESValueParserFailed(name + "\n" + f.toString)
    }

  private def getOrElse[T](
    name: String,
    rule: Parser[T],
    str: String,
    default: T,
  ): T = parseAll(rule, str) match {
    case Success(res, _) => res
    case _               => default
  }

  // remove numeric literal separator
  private def noSep(s: String): String = s.replace("_", "")

  // check if a string is an octal string
  private def isOctalString(s: String): Boolean =
    s.startsWith("0") && s.forall(c => '0' <= c && c < '8')
}
