package esmeta.js.util

import esmeta.js.*
import esmeta.interp.{BigInt => BigIntV, *}
import esmeta.error.*
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.*

object ESValueParser extends UnicodeParsers with RegexParsers {
  // empty data for EPackratParser
  case class Data() extends DataType { def next = this }
  def defaultData = Data()

  // parsing
  def parseIdentifier(str: String): String =
    get("SV.IdentifierName", SV.IdentifierName, str)
  def parseString(str: String): String =
    get("SV.StringLiteral", SV.StringLiteral, str)
  def parseNumber(str: String): LiteralValue =
    get("NumericValue.NumericLiteral", NumericValue.NumericLiteral, str)
  def parseTVNoSubstitutionTemplate(str: String): String =
    get("TV.NoSubstitutionTemplate", TV.NoSubstitutionTemplate, str)
  def parseTRVNoSubstitutionTemplate(str: String): String =
    getOrElse("TRV.NoSubstitutionTemplate", TRV.NoSubstitutionTemplate, str, "")
  def parseTVTemplateHead(str: String): String =
    get("TV.TemplateHead", TV.TemplateHead, str)
  def parseTRVTemplateHead(str: String): String =
    get("TRV.TemplateHead", TRV.TemplateHead, str)
  def parseTVTemplateMiddle(str: String): String =
    get("TV.TemplateMiddle", TV.TemplateMiddle, str)
  def parseTRVTemplateMiddle(str: String): String =
    get("TRV.TemplateMiddle", TRV.TemplateMiddle, str)
  def parseTVTemplateTail(str: String): String =
    get("TV.TemplateTail", TV.TemplateTail, str)
  def parseTRVTemplateTail(str: String): String =
    get("TRV.TemplateTail", TRV.TemplateTail, str)
  def str2Number(str: String): Double =
    parseAll(DoubleMV.StringNumericLiteral, str) match {
      case Success(0, _) if str.trim.startsWith("-") => -0.0
      case Success(n, _)                             => n.toDouble
      case _                                         => Double.NaN
    }
  def str2bigint(str: String): LiteralValue =
    parseAll(BigIntMV.StringNumericLiteralForBigInt, str) match {
      case Success(b, _) => BigIntV(b)
      case _             => Undef
    }
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

  // String Value
  object SV {
    lazy val IdentifierName: S = (
      seq(
        SV.IdentifierStart,
        rep(SV.IdentifierPart) ^^ { case l => l.foldLeft("")(_ + _) },
      ),
    )
    lazy val IdentifierStart: S = (
      IDStart |||
        "$" |||
        "_" |||
        "\\" ~> SV.UnicodeEscapeSequence
    )
    lazy val IdentifierPart: S = (
      IDContinue |||
        "$" |||
        "\\" ~> SV.UnicodeEscapeSequence |||
        Predef.ZWNJ |||
        Predef.ZWJ
    )
    lazy val StringLiteral: S = (
      // The SV of StringLiteral::"" is the empty code unit sequence.
      "\"\"" ^^^ "" |||
        // The SV of StringLiteral::'' is the empty code unit sequence.
        "''" ^^^ "" |||
        // The SV of StringLiteral::"DoubleStringCharacters" is the SV of DoubleStringCharacters.
        "\"" ~> SV.DoubleStringCharacters <~ "\"" |||
        // The SV of StringLiteral::'SingleStringCharacters' is the SV of SingleStringCharacters.
        "'" ~> SV.SingleStringCharacters <~ "'"
    )
    lazy val DoubleStringCharacters: S = (
      // The SV of DoubleStringCharacters::DoubleStringCharacter is a sequence of up to two code units that is the SV of DoubleStringCharacter.
      SV.DoubleStringCharacter |||
        // The SV of DoubleStringCharacters::DoubleStringCharacterDoubleStringCharacters is a sequence of up to two code units that is the SV of DoubleStringCharacter followed by the code units of the SV of DoubleStringCharacters in order.
        seq(SV.DoubleStringCharacter, SV.DoubleStringCharacters)
    )
    lazy val SingleStringCharacters: S = (
      // The SV of SingleStringCharacters::SingleStringCharacter is a sequence of up to two code units that is the SV of SingleStringCharacter.
      SV.SingleStringCharacter |||
        // The SV of SingleStringCharacters::SingleStringCharacterSingleStringCharacters is a sequence of up to two code units that is the SV of SingleStringCharacter followed by the code units of the SV of SingleStringCharacters in order.
        seq(SV.SingleStringCharacter, SV.SingleStringCharacters)
    )
    lazy val DoubleStringCharacter: S = (
      // The SV of DoubleStringCharacter::SourceCharacterbut not one of " or \ or LineTerminator is the UTF16Encoding of the code point value of SourceCharacter.
      notChars("\"" | "\\" | Predef.LineTerminator) |||
        // The SV of DoubleStringCharacter::<LS> is the code unit 0x2028 (LINE SEPARATOR).
        Predef.LS |||
        // The SV of DoubleStringCharacter::<PS> is the code unit 0x2029 (PARAGRAPH SEPARATOR)
        Predef.PS |||
        // The SV of DoubleStringCharacter::\EscapeSequence is the SV of the EscapeSequence.
        "\\" ~> SV.EscapeSequence |||
        // The SV of DoubleStringCharacter::LineContinuation is the empty code unit sequence.
        Predef.LineContinuation ^^^ ""
    )
    lazy val SingleStringCharacter: S = (
      // The SV of SingleStringCharacter::SourceCharacterbut not one of ' or \ or LineTerminator is the UTF16Encoding of the code point value of SourceCharacter.
      notChars("'" | "\\" | Predef.LineTerminator) |||
        // The SV of SingleStringCharacter::<LS> is the code unit 0x2028 (LINE SEPARATOR).
        Predef.LS |||
        // The SV of SingleStringCharacter::<PS> is the code unit 0x2029 (PARAGRAPH SEPARATOR).
        Predef.PS |||
        // The SV of SingleStringCharacter::\EscapeSequence is the SV of the EscapeSequence.
        "\\" ~> SV.EscapeSequence |||
        // The SV of SingleStringCharacter::LineContinuation is the empty code unit sequence.
        Predef.LineContinuation ^^^ ""
    )
    lazy val EscapeSequence: S = (
      // The SV of EscapeSequence::CharacterEscapeSequence is the SV of the CharacterEscapeSequence.
      SV.CharacterEscapeSequence |||
        // The SV of EscapeSequence::0 is the code unit 0x0000 (NULL).
        "0" ^^^ "\u0000" |||
        // The SV of EscapeSequence::HexEscapeSequence is the SV of the HexEscapeSequence.
        SV.HexEscapeSequence |||
        // The SV of EscapeSequence::UnicodeEscapeSequence is the SV of the UnicodeEscapeSequence.
        SV.UnicodeEscapeSequence
    )
    lazy val CharacterEscapeSequence: S = (
      // The SV of CharacterEscapeSequence::SingleEscapeCharacter is the code unit whose value is determined by the SingleEscapeCharacter according to Table 34.
      Predef.SingleEscapeCharacter ^^ {
        case "b" => "\u0008"
        case "t" => "\u0009"
        case "n" => "\u000a"
        case "v" => "\u000b"
        case "f" => "\u000c"
        case "r" => "\u000d"
        case s   => s
      } |||
        // The SV of CharacterEscapeSequence::NonEscapeCharacter is the SV of the NonEscapeCharacter.
        SV.NonEscapeCharacter
    )
    lazy val NonEscapeCharacter: S = (
      // The SV of NonEscapeCharacter::SourceCharacterbut not one of EscapeCharacter or LineTerminator is the UTF16Encoding of the code point value of SourceCharacter.
      notChars(Predef.EscapeCharacter | Predef.LineTerminator),
    )
    lazy val HexEscapeSequence: S = (
      // The SV of HexEscapeSequence::xHexDigitHexDigit is the code unit whose value is (16 times the MV of the first HexDigit) plus the MV of the second HexDigit.
      "x" ~> DoubleMV.HexDigit ~ DoubleMV.HexDigit ^^ {
        case x ~ y => Character.toChars(16 * x.toInt + y.toInt).mkString
      }
    )
    lazy val UnicodeEscapeSequence: S = (
      // The SV of UnicodeEscapeSequence::uHex4Digits is the SV of Hex4Digits.
      "u" ~> SV.Hex4Digits |||
        // The SV of UnicodeEscapeSequence::u{CodePoint} is the UTF16Encoding of the MV of CodePoint(HexDigits).
        "u{" ~> DoubleMV.CodePoint <~ "}" ^^ {
          case n => Character.toChars(n.toInt).mkString
        }
    )
    lazy val Hex4Digits: S = (
      // The SV of Hex4Digits::HexDigitHexDigitHexDigitHexDigit is the code unit whose value is (0x1000 times the MV of the first HexDigit) plus (0x100 times the MV of the second HexDigit) plus (0x10 times the MV of the third HexDigit) plus the MV of the fourth HexDigit.
      DoubleMV.HexDigit ~ DoubleMV.HexDigit ~ DoubleMV.HexDigit ~ DoubleMV.HexDigit ^^ {
        case a ~ b ~ c ~ d =>
          Character
            .toChars(
              a.toInt * 0x1000 +
              b.toInt * 0x100 +
              c.toInt * 0x10 +
              d.toInt * 0x1,
            )
            .mkString
      }
    )
  }

  // Numeric Value
  object NumericValue {
    lazy val NumericLiteral: V = (
      DoubleMV.DecimalLiteral ^^ { Number(_) } |||
        NumericValue.DecimalBigIntegerLiteral |||
        DoubleMV.NonDecimalIntegerLiteral ^^ { Number(_) } |||
        BigIntMV.NonDecimalIntegerLiteral <~ Predef.BigIntLiteralSuffix ^^ {
          BigIntV(_)
        }
    )

    lazy val DecimalBigIntegerLiteral: V =
      ("0" ||| Predef.NonZeroDigit ~ opt(Predef.DecimalDigits) ^^ {
        case x ~ y => x + y.getOrElse("")
      }) <~ Predef.BigIntLiteralSuffix ^^ { s => BigIntV(BigInt(s)) }
  }

  // Mathematical Value
  object DoubleMV
    extends MV[Double](0, 1, _.toDouble, _.toDouble, _ + _, _ * _, _ < _)
  object BigIntMV
    extends MV[BigInt](0, 1, BigInt(_), BigInt(_), _ + _, _ * _, _ < _)
  case class MV[T](
    zero: T,
    one: T,
    fromInt: Int => T,
    fromString: String => T,
    add: (T, T) => T,
    mul: (T, T) => T,
    lt: (T, T) => Boolean,
  ) {
    type D = Parser[T]
    lazy val DecimalLiteral: D = Predef.DecimalLiteral ^^ fromString
    lazy val NonDecimalIntegerLiteral: D = (
      this.BinaryIntegerLiteral |||
        this.OctalIntegerLiteral |||
        this.HexIntegerLiteral
    )
    lazy val NonZeroDigit: D = Predef.NonZeroDigit ^^ fromString
    lazy val DecimalDigits: D = Predef.DecimalDigits ^^ fromString
    lazy val BinaryIntegerLiteral: D = ("0b" | "0B") ~> this.BinaryDigits
    lazy val BinaryDigits: D = rep1("0" | "1") ^^ {
      case list =>
        list.foldLeft(zero) {
          case (x, s) => add(mul(x, fromInt(2)), fromString(s))
        }
    }
    lazy val OctalIntegerLiteral: D = ("0o" | "0O") ~> this.OctalDigits
    lazy val OctalDigits: D = rep1("[0-7]".r) ^^ {
      case list =>
        list.foldLeft(zero) {
          case (x, s) => add(mul(x, fromInt(8)), fromString(s))
        }
    }
    lazy val HexIntegerLiteral: D = ("0x" | "0X") ~> this.HexDigits
    lazy val HexDigits: D = rep1(HexDigit) ^^ {
      case list =>
        list.foldLeft(zero) {
          case (x, y) => add(mul(x, fromInt(16)), y)
        }
    }
    lazy val StringNumericLiteral: D = (
      opt(Predef.StrWhiteSpace) ^^^ zero |||
        opt(Predef.StrWhiteSpace) ~> this.StrNumericLiteral <~ opt(
          Predef.StrWhiteSpace,
        )
    )
    lazy val StrNumericLiteral: D = (
      this.StrDecimalLiteral |||
        this.NonDecimalIntegerLiteral
    )
    lazy val StrDecimalLiteral: D = Predef.StrDecimalLiteral ^^ fromString
    lazy val StringNumericLiteralForBigInt: D = (
      opt(Predef.StrWhiteSpace) ^^^ zero |||
        opt(Predef.StrWhiteSpace) ~> this.StrNumericLiteralForBigInt <~ opt(
          Predef.StrWhiteSpace,
        )
    )
    lazy val StrNumericLiteralForBigInt: D = (
      this.StrDecimalLiteralForBigInt |||
        this.NonDecimalIntegerLiteral
    )
    lazy val StrDecimalLiteralForBigInt: D =
      Predef.StrDecimalLiteralForBigInt ^^ fromString
    lazy val CodePoint: D =
      this.HexDigits.filter(d => !lt(fromInt(0x10ffff), d))
    lazy val HexDigit: D = "[0-9a-fA-F]".r ^^ {
      case s =>
        val ch = s.head
        fromInt({
          if (ch.isUpper) ch - 'A' + 10
          else if (ch.isLower) ch - 'a' + 10
          else ch - '0'
        })
    }
  }

  // Template Value
  object TV {
    lazy val NoSubstitutionTemplate: S = (
      // The TV of NoSubstitutionTemplate::`` is the empty code unit sequence.
      "``" ^^^ "" |||
        // The TV of NoSubstitutionTemplate::`TemplateCharacters` is the TV of TemplateCharacters.
        "`" ~> TV.TemplateCharacters <~ "`"
    )
    lazy val TemplateHead: S = (
      // The TV of TemplateHead::`${ is the empty code unit sequence.
      "`${" ^^^ "" |||
        // The TV of TemplateHead::`TemplateCharacters${ is the TV of TemplateCharacters.
        "`" ~> TV.TemplateCharacters <~ "${"
    )
    lazy val TemplateMiddle: S = (
      // The TV of TemplateMiddle::}${ is the empty code unit sequence.
      "}${" ^^^ "" |||
        // The TV of TemplateMiddle::}TemplateCharacters${ is the TV of TemplateCharacters.
        "}" ~> TV.TemplateCharacters <~ "${"
    )
    lazy val TemplateTail: S = (
      // The TV of TemplateTail::}TemplateCharacters` is the TV of TemplateCharacters.
      "}" ~> TV.TemplateCharacters <~ "`" |||
        // The TV of TemplateTail::}` is the empty code unit sequence.
        "}`" ^^^ ""
    )
    lazy val TemplateCharacter: S = (
      // The TV of TemplateCharacter::SourceCharacterbut not one of ` or \ or $ or LineTerminator is the UTF16Encoding of the code point value of SourceCharacter.
      notChars("`" | "\\" | "$" | Predef.LineTerminator) |||
        // The TV of TemplateCharacter::$ is the code unit 0x0024 (DOLLAR SIGN).
        "$" <~ not("{") |||
        // The TV of TemplateCharacter::\EscapeSequence is the SV of EscapeSequence.
        "\\" ~> SV.EscapeSequence |||
        // The TV of TemplateCharacter::\NotEscapeSequence is undefined.
        "\\" ~> Predef.NotEscapeSequence ^^^ {
          throw ESValueParserFailed("")
        } |||
        // The TV of TemplateCharacter::LineContinuation is the TV of LineContinuation.
        TV.LineContinuation |||
        // The TV of TemplateCharacter::LineTerminatorSequence is the TRV of LineTerminatorSequence.
        TRV.LineTerminatorSequence
    )
    lazy val TemplateCharacters: S = (
      // The TV of TemplateCharacters::TemplateCharacter is the TV of TemplateCharacter.
      TV.TemplateCharacter |||
        // Otherwise, it is a sequence consisting of the code units of the TV of TemplateCharacter followed by the code units of the TV of TemplateCharacters.
        seq(TemplateCharacter, TemplateCharacters)
    )
    lazy val LineContinuation: S = (
      // The TV of LineContinuation::\LineTerminatorSequence is the empty code unit sequence.
      "\\" ~ Predef.LineTerminatorSequence ^^^ ""
    )
  }

  // Template Raw Value
  object TRV {
    lazy val CharacterEscapeSequence: S = (
      // The TRV of CharacterEscapeSequence::NonEscapeCharacter is the SV of the NonEscapeCharacter.
      SV.NonEscapeCharacter |||
        // The TRV of CharacterEscapeSequence::SingleEscapeCharacter is the TRV of the SingleEscapeCharacter.
        TRV.SingleEscapeCharacter
    )
    lazy val DecimalDigit: S = (
      // The TRV of DecimalDigit::one of0123456789 is the SV of the SourceCharacter that is that single code point.
      "[0-9]".r
    )
    lazy val EscapeSequence: S = (
      // The TRV of EscapeSequence::CharacterEscapeSequence is the TRV of the CharacterEscapeSequence.
      TRV.CharacterEscapeSequence |||
        // The TRV of EscapeSequence::0 is the code unit 0x0030 (DIGIT ZERO).
        "0" <~ not(Predef.DecimalDigit) |||
        // The TRV of EscapeSequence::HexEscapeSequence is the TRV of the HexEscapeSequence.
        TRV.HexEscapeSequence |||
        // The TRV of EscapeSequence::UnicodeEscapeSequence is the TRV of the UnicodeEscapeSequence.
        TRV.UnicodeEscapeSequence
    )
    lazy val Hex4Digits: S = (
      // The TRV of Hex4Digits::HexDigitHexDigitHexDigitHexDigit is the sequence consisting of the TRV of the first HexDigit followed by the TRV of the second HexDigit followed by the TRV of the third HexDigit followed by the TRV of the fourth HexDigit.
      seq(
        TRV.HexDigit,
        TRV.HexDigit,
        TRV.HexDigit,
        TRV.HexDigit,
      ),
    )
    lazy val NotCodePoint: S = Predef.NotCodePoint
    lazy val CodePoint: S = Predef.CodePoint
    lazy val HexDigits: S = (
      // The TRV of HexDigits::HexDigit is the TRV of HexDigit.
      TRV.HexDigit |||
        // The TRV of HexDigits::HexDigitsHexDigit is the sequence consisting of TRV of HexDigits followed by TRV of HexDigit.
        seq(TRV.HexDigit, TRV.HexDigits)
    )
    lazy val HexEscapeSequence: S = (
      // The TRV of HexEscapeSequence::xHexDigitHexDigit is the sequence consisting of the code unit 0x0078 (LATIN SMALL LETTER X) followed by TRV of the first HexDigit followed by the TRV of the second HexDigit.
      seq(
        "x",
        TRV.HexDigit,
        TRV.HexDigit,
      ),
    )
    lazy val LineContinuation: S = (
      // The TRV of LineContinuation::\LineTerminatorSequence is the sequence consisting of the code unit 0x005C (REVERSE SOLIDUS) followed by the code units of TRV of LineTerminatorSequence.
      seq(
        "\\",
        LineTerminatorSequence,
      ),
    )
    lazy val LineTerminatorSequence: S = (
      // The TRV of LineTerminatorSequence::<CR> is the code unit 0x000A (LINE FEED).
      Predef.CR ^^^ "\u000a" |||
        // The TRV of LineTerminatorSequence::<CR><LF> is the sequence consisting of the code unit 0x000A (LINE FEED).
        Predef.CR ~> Predef.LF |||
        // The TRV of LineTerminatorSequence::<LF> is the code unit 0x000A (LINE FEED).
        Predef.LF |||
        // The TRV of LineTerminatorSequence::<LS> is the code unit 0x2028 (LINE SEPARATOR).
        Predef.LS |||
        // The TRV of LineTerminatorSequence::<PS> is the code unit 0x2029 (PARAGRAPH SEPARATOR).
        Predef.PS
    )
    lazy val NoSubstitutionTemplate: S = (
      // The TRV of NoSubstitutionTemplate::`TemplateCharacters` is the TRV of TemplateCharacters.
      "`" ~> TRV.TemplateCharacters <~ "`" |||
        // The TRV of NoSubstitutionTemplate::`` is the empty code unit sequence.
        "``" ^^^ ""
    )
    lazy val NotEscapeSequence: S = (
      // The TRV of NotEscapeSequence::0DecimalDigit is the sequence consisting of the code unit 0x0030 (DIGIT ZERO) followed by the code units of the TRV of DecimalDigit.
      seq("0", TRV.DecimalDigit) |||
        // The TRV of NotEscapeSequence::DecimalDigit not 0 is the code units of the TRV of DecimalDigit.
        TRV.DecimalDigit.filter(_ != "0") |||
        // The TRV of NotEscapeSequence::uHexDigitHexDigitHexDigit[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code units of the TRV of the first HexDigit followed by the code units of the TRV of the second HexDigit followed by the code units of the TRV of the third HexDigit.
        seq("u", TRV.HexDigit, TRV.HexDigit, TRV.HexDigit) <~ not(
          Predef.HexDigit,
        ) |||
        // The TRV of NotEscapeSequence::uHexDigitHexDigit[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code units of the TRV of the first HexDigit followed by the code units of the TRV of the second HexDigit.
        seq("u", TRV.HexDigit, TRV.HexDigit) <~ not(Predef.HexDigit) |||
        // The TRV of NotEscapeSequence::uHexDigit[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code units of the TRV of HexDigit.
        seq("u", TRV.HexDigit) <~ not(Predef.HexDigit) |||
        // The TRV of NotEscapeSequence::u[lookahead ∉ HexDigit][lookahead ≠ {] is the code unit 0x0075 (LATIN SMALL LETTER U).
        "u" <~ not(Predef.HexDigit) <~ not("{") |||
        // The TRV of NotEscapeSequence::u{CodePoint[lookahead ∉ HexDigit][lookahead ≠ }] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code unit 0x007B (LEFT CURLY BRACKET) followed by the code units of the TRV of CodePoint.
        seq("u{", TRV.CodePoint) <~ not(Predef.HexDigit) <~ not("}") |||
        // The TRV of NotEscapeSequence::u{NotCodePoint[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code unit 0x007B (LEFT CURLY BRACKET) followed by the code units of the TRV of NotCodePoint.
        seq("u{", TRV.NotCodePoint) <~ not(Predef.HexDigit) |||
        // The TRV of NotEscapeSequence::u{[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code unit 0x007B (LEFT CURLY BRACKET).
        "u{" <~ not(Predef.HexDigit) |||
        // The TRV of NotEscapeSequence::xHexDigit[lookahead ∉ HexDigit] is the sequence consisting of the code unit 0x0078 (LATIN SMALL LETTER X) followed by the code units of the TRV of HexDigit.
        seq("x", TRV.HexDigit) <~ not(HexDigit) |||
        // The TRV of NotEscapeSequence::x[lookahead ∉ HexDigit] is the code unit 0x0078 (LATIN SMALL LETTER X).
        "x" <~ not(HexDigit)
    )
    lazy val SingleEscapeCharacter: S = (
      // The TRV of SingleEscapeCharacter::one of'"\bfnrtv is the SV of the SourceCharacter that is that single code point.
      Predef.SingleEscapeCharacter
    )
    lazy val TemplateCharacter: S = (
      // The TRV of TemplateCharacter::$ is the code unit 0x0024 (DOLLAR SIGN).
      "$" <~ not("{") |||
        // The TRV of TemplateCharacter::LineContinuation is the TRV of LineContinuation.
        TRV.LineContinuation |||
        // The TRV of TemplateCharacter::LineTerminatorSequence is the TRV of LineTerminatorSequence.
        TRV.LineTerminatorSequence |||
        // The TRV of TemplateCharacter::SourceCharacterbut not one of ` or \ or $ or LineTerminator is the UTF16Encoding of the code point value of SourceCharacter.
        notChars("`" | "\\" | "$" | Predef.LineTerminator) |||
        // The TRV of TemplateCharacter::\EscapeSequence is the sequence consisting of the code unit 0x005C (REVERSE SOLIDUS) followed by the code units of TRV of EscapeSequence.
        seq("\\", TRV.EscapeSequence) |||
        // The TRV of TemplateCharacter::\NotEscapeSequence is the sequence consisting of the code unit 0x005C (REVERSE SOLIDUS) followed by the code units of TRV of NotEscapeSequence.
        seq("\\", TRV.NotEscapeSequence)
    )
    lazy val TemplateCharacters: S = (
      // The TRV of TemplateCharacters::TemplateCharacter is the TRV of TemplateCharacter.
      TRV.TemplateCharacter |||
        // The TRV of TemplateCharacters::TemplateCharacterTemplateCharacters is a sequence consisting of the code units of the TRV of TemplateCharacter followed by the code units of the TRV of TemplateCharacters.
        seq(TRV.TemplateCharacter, TRV.TemplateCharacters)
    )
    lazy val TemplateHead: S = (
      // The TRV of TemplateHead::`${ is the empty code unit sequence.
      "`${" ^^^ "" |||
        // The TRV of TemplateHead::`TemplateCharacters${ is the TRV of TemplateCharacters.
        "`" ~> TRV.TemplateCharacters <~ "${"
    )
    lazy val TemplateMiddle: S = (
      // The TRV of TemplateMiddle::}${ is the empty code unit sequence.
      "}${" ^^^ "" |||
        // The TRV of TemplateMiddle::}TemplateCharacters${ is the TRV of TemplateCharacters.
        "}" ~> TRV.TemplateCharacters <~ "${"
    )
    lazy val TemplateTail: S = (
      // The TRV of TemplateTail::}TemplateCharacters` is the TRV of TemplateCharacters.
      "}" ~> TRV.TemplateCharacters <~ "`" |||
        // The TRV of TemplateTail::}` is the empty code unit sequence.
        "}`" ^^^ ""
    )
    lazy val UnicodeEscapeSequence: S = (
      // The TRV of UnicodeEscapeSequence::uHex4Digits is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by TRV of Hex4Digits.
      seq("u", TRV.Hex4Digits) |||
        // The TRV of UnicodeEscapeSequence::u{CodePoint} is the sequence consisting of the code unit 0x0075 (LATIN SMALL LETTER U) followed by the code unit 0x007B (LEFT CURLY BRACKET) followed by TRV of CodePoint followed by the code unit 0x007D (RIGHT CURLY BRACKET).
        seq("u{", TRV.CodePoint, "}")
    )
    lazy val HexDigit: S = (
      // The TRV of a HexDigit is the SV of the SourceCharacter that is that HexDigit.
      Predef.HexDigit
    )
  }

  // types
  type S = Parser[String]
  type V = Parser[LiteralValue]

  // predefined parsers
  object Predef {
    lazy val SourceCharacter: S = "(?s).".r
    lazy val ZWNJ: Parser[String] = "\u200C"
    lazy val ZWJ: Parser[String] = "\u200D"
    lazy val ZWNBSP: Parser[String] = "\uFEFF"
    lazy val TAB: Parser[String] = "\u0009"
    lazy val VT: Parser[String] = "\u000B"
    lazy val FF: Parser[String] = "\u000C"
    lazy val SP: Parser[String] = "\u0020"
    lazy val NBSP: Parser[String] = "\u00A0"
    lazy val USP: Parser[String] =
      "[\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]".r
    lazy val LF: S = "\u000A"
    lazy val CR: S = "\u000D"
    lazy val LS: S = "\u2028"
    lazy val PS: S = "\u2029"
    lazy val WhiteSpace: Parser[String] =
      TAB | VT | FF | SP | NBSP | ZWNBSP | USP
    lazy val LineTerminator: S = LF | CR | LS | PS
    lazy val LineTerminatorSequence: Parser[String] =
      LF | CR <~ not(LF) | LS | PS | seq(CR, LF)
    lazy val LineContinuation: S = "\\" ~> LineTerminatorSequence
    lazy val SingleEscapeCharacter: S = """['"\\bfnrtv]""".r
    lazy val EscapeCharacter: S = (
      SingleEscapeCharacter |||
        DecimalDigit |||
        "x" |||
        "u"
    )
    lazy val NotEscapeSequence: S = (
      seq("0", DecimalDigit) |||
        DecimalDigit.filter(_ != "0") |||
        "x" <~ not(HexDigit) |||
        seq("x", HexDigit <~ not(HexDigit)) |||
        "u" <~ not(HexDigit | "{") |||
        seq("u", HexDigit <~ not(HexDigit)) |||
        seq("u", HexDigit, HexDigit <~ not(HexDigit)) |||
        seq("u", HexDigit, HexDigit, HexDigit <~ not(HexDigit)) |||
        "u{" <~ not(HexDigit) |||
        seq("u{", NotCodePoint <~ not(HexDigit)) |||
        seq("u{", CodePoint <~ not(HexDigit | "}"))
    )
    lazy val DecimalLiteral: S = (
      seq(
        DecimalIntegerLiteral,
        ".",
        sOpt(DecimalDigits),
        sOpt(ExponentPart),
      ) |||
        seq(".", DecimalDigits, sOpt(ExponentPart)) |||
        seq(DecimalIntegerLiteral, sOpt(ExponentPart))
    )
    lazy val StrDecimalLiteral: S = (
      StrUnsignedDecimalLiteral |||
        seq("+", StrUnsignedDecimalLiteral) |||
        seq("-", StrUnsignedDecimalLiteral)
    )
    lazy val StrDecimalLiteralForBigInt: S = (
      DecimalDigits |||
        seq("+", DecimalDigits) |||
        seq("-", DecimalDigits)
    )
    lazy val StrUnsignedDecimalLiteral: S = (
      "Infinity" ^^^ "1e10000" |||
        seq(DecimalDigits, ".", sOpt(DecimalDigits), sOpt(ExponentPart)) |||
        seq(".", DecimalDigits, sOpt(ExponentPart)) |||
        seq(DecimalDigits, sOpt(ExponentPart))
    )
    lazy val DecimalIntegerLiteral: S = (
      "0" |||
        seq(NonZeroDigit, sOpt(DecimalDigits))
    )
    lazy val DecimalDigits: S = (
      DecimalDigit |||
        seq(DecimalDigit, DecimalDigits)
    )
    lazy val DecimalDigit: S = "[0-9]".r
    lazy val NonZeroDigit: S = "[1-9]".r
    lazy val ExponentPart: S = (
      seq(ExponentIndicator, SignedInteger),
    )
    lazy val ExponentIndicator: S = "[eE]".r
    lazy val SignedInteger: S = (
      DecimalDigits |||
        seq("+", DecimalDigits) |||
        seq("-", DecimalDigits)
    )
    lazy val StrWhiteSpace: S = (
      seq(StrWhiteSpaceChar, sOpt(StrWhiteSpace)),
    )
    lazy val StrWhiteSpaceChar: S = (
      WhiteSpace |||
        LineTerminator
    )
    lazy val HexDigits: S = seq(HexDigit, sOpt(HexDigits))
    lazy val HexDigit: S = "[0-9a-fA-F]".r
    lazy val CodePoint: S = HexDigits.filter {
      case s => parseAll(DoubleMV.HexDigits, s).get <= 0x10ffff
    }
    lazy val NotCodePoint: S = HexDigits.filter {
      case s => parseAll(DoubleMV.HexDigits, s).get > 0x10ffff
    }
    lazy val BigIntLiteralSuffix: S = "n"
  }

  // sequences
  def sOpt(s: => S): S = opt(s) ^^ {
    case Some(s) => s
    case None    => ""
  }
  def seq(x0: S, x1: => S): S = x0 ~ x1 ^^ { case x ~ y => x + y }
  def seq(x0: S, x1: => S, x2: => S): S = x0 ~ x1 ~ x2 ^^ {
    case x ~ y ~ z => x + y + z
  }
  def seq(x0: S, x1: => S, x2: => S, x3: => S): S = x0 ~ x1 ~ x2 ~ x3 ^^ {
    case x ~ y ~ z ~ a => x + y + z + a
  }

  // filtering out source characters
  def notChars(cond: S): S =
    Predef.SourceCharacter.filter(s => parseAll(cond, s).isEmpty)

  // not skip white spaces
  override def skipWhitespace = false
}
