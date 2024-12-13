package esmeta
package ir
package util

import cats.syntax.functor.*

import esmeta.lang.Syntax
import esmeta.lang.util.JsonProtocol.given
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import scala.util.Try
import javax.print.Doc
import esmeta.es.builtin.T

object JsonProtocolWithBackEdge extends BasicJsonProtocol {
  val stringifier = IRElem.getStringifier(true, false)
  import stringifier.given
  lazy val LANG_OPT_LOC = "langOptLoc"

  given Encoder[Type] = encoderWithStringifier(stringify)
  given Decoder[Type] = decoderWithParser(Type.from)

  given Encoder[Ref] = encoderWithStringifier(stringify)
  given Decoder[Ref] = decoderWithParser(Ref.from)
  given Encoder[Var] = encoderWithStringifier(stringify)
  given Decoder[Var] = decoderWithOptionParser(Ref.from.map {
    case x: Var => Some(x)
    case _      => None
  })
  given Encoder[Name] = encoderWithStringifier(stringify)
  given Decoder[Name] = decoderWithOptionParser(Ref.from.map {
    case x: Name => Some(x)
    case _       => None
  })
  given Encoder[Temp] = encoderWithStringifier(stringify)
  given Decoder[Temp] = decoderWithOptionParser(Ref.from.map {
    case x: Temp => Some(x)
    case _       => None
  })
  given Encoder[Global] = encoderWithStringifier(stringify)
  given Decoder[Global] = decoderWithOptionParser(Ref.from.map {
    case x: Global => Some(x)
    case _         => None
  })
  given Encoder[Local] = encoderWithStringifier(stringify)
  given Decoder[Local] = decoderWithOptionParser(Ref.from.map {
    case x: Local => Some(x)
    case _        => None
  })
  given Encoder[Field] = encoderWithStringifier(stringify)
  given Decoder[Field] = decoderWithOptionParser(Ref.from.map {
    case x: Field => Some(x)
    case _        => None
  })

  /* ///////////////////   Expressions   //////////////////// */

  given exprEncoder: Encoder[Expr] = Encoder.instance {
    case x: EParse         => x.asJson
    case x: EGrammarSymbol => x.asJson
    case x: ESourceText    => x.asJson
    case x: EYet           => x.asJson
    case x: EContains      => x.asJson
    case x: ESubstring     => x.asJson
    case x: ETrim          => x.asJson
    case x: ERef           => x.asJson
    case x: EUnary         => x.asJson
    case x: EBinary        => x.asJson
    case x: EVariadic      => x.asJson
    case x: EMathOp        => x.asJson
    case x: EConvert       => x.asJson
    case x: EExists        => x.asJson
    case x: ETypeOf        => x.asJson
    case x: EInstanceOf    => x.asJson
    case x: ETypeCheck     => x.asJson
    case x: ESizeOf        => x.asJson
    case x: EClo           => x.asJson
    case x: ECont          => x.asJson
    case x: EDebug         => x.asJson
    case x: ERandom        => x.asJson
    case x: ESyntactic     => x.asJson
    case x: ELexical       => x.asJson
    case x: ERecord        => x.asJson
    case x: EMap           => x.asJson
    case x: EList          => x.asJson
    case x: ECopy          => x.asJson
    case x: EKeys          => x.asJson
    case x: EMath          => x.asJson
    case x: EInfinity      => x.asJson
    case x: ENumber        => x.asJson
    case x: EBigInt        => x.asJson
    case x: EStr           => x.asJson
    case x: EBool          => x.asJson
    case x: EUndef         => x.asJson
    case x: ENull          => x.asJson
    case x: EEnum          => x.asJson
    case x: ECodeUnit      => x.asJson
  }

  var instanceCounter = 0

  given exprDecoder: Decoder[Expr] = {
    instanceCounter += 1;
    Decoder.recursive { implicit recurse =>
      List[Decoder[Expr]](
        Decoder[EParse].widen,
        Decoder[EGrammarSymbol].widen,
        Decoder[ESourceText].widen,
        Decoder[EYet].widen,
        Decoder[EContains].widen,
        Decoder[ESubstring].widen,
        Decoder[ETrim].widen,
        Decoder[ERef].widen,
        Decoder[EUnary].widen,
        Decoder[EBinary].widen,
        Decoder[EVariadic].widen,
        Decoder[EMathOp].widen,
        Decoder[EConvert].widen,
        Decoder[EExists].widen,
        Decoder[ETypeOf].widen,
        Decoder[EInstanceOf].widen,
        Decoder[ETypeCheck].widen,
        Decoder[ESizeOf].widen,
        Decoder[EClo].widen,
        Decoder[ECont].widen,
        Decoder[EDebug].widen,
        Decoder[ERandom].widen,
        Decoder[ESyntactic].widen,
        Decoder[ELexical].widen,
        Decoder[ERecord].widen,
        Decoder[EMap].widen,
        Decoder[EList].widen,
        Decoder[ECopy].widen,
        Decoder[EKeys].widen,
        Decoder[EMath].widen,
        Decoder[EInfinity].widen,
        Decoder[ENumber].widen,
        Decoder[EBigInt].widen,
        Decoder[EStr].widen,
        Decoder[EBool].widen,
        Decoder[EUndef].widen,
        Decoder[ENull].widen,
        Decoder[EEnum].widen,
        Decoder[ECodeUnit].widen,
      ).reduce(_ or _)
    }
  }

  given (using Decoder[Expr]): Decoder[EParse] = withLoc("expr") {
    Decoder.forProduct2[EParse, Expr, Expr]("code", "rule")(EParse.apply)
  }
  given Decoder[EGrammarSymbol] =
    withLoc("expr") {
      Decoder.forProduct2[EGrammarSymbol, String, List[Boolean]](
        "name",
        "params",
      )(
        EGrammarSymbol.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ESourceText] =
    withLoc("expr") {
      Decoder.forProduct1[ESourceText, Expr]("expr")(ESourceText.apply)
    }
  given Decoder[EYet] = withLoc("inst") {
    Decoder.forProduct1[EYet, String]("msg")(EYet.apply)
  }
  given (using Decoder[Expr]): Decoder[EContains] =
    withLoc("expr") {
      Decoder.forProduct2[EContains, Expr, Expr]("list", "expr")(
        EContains.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ESubstring] =
    withLoc("expr") {
      Decoder.forProduct3[ESubstring, Expr, Expr, Option[Expr]](
        "expr",
        "from",
        "to",
      )(
        ESubstring.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ETrim] = withLoc("expr") {
    Decoder.forProduct2[ETrim, Expr, Boolean]("expr", "isStarting")(ETrim.apply)
  }
  given Decoder[ERef] = withLoc("expr") {
    Decoder.forProduct1[ERef, Ref]("ref")(ERef.apply)
  }
  given (using Decoder[Expr]): Decoder[EUnary] = withLoc("expr") {
    Decoder.forProduct2[EUnary, UOp, Expr]("uop", "expr")(EUnary.apply)
  }
  given (using Decoder[Expr]): Decoder[EBinary] =
    withLoc("expr") {
      Decoder.forProduct3[EBinary, BOp, Expr, Expr](
        "bop",
        "left",
        "right",
      )(
        EBinary.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[EVariadic] =
    withLoc("expr") {
      Decoder.forProduct2[EVariadic, VOp, List[Expr]]("vop", "exprs")(
        EVariadic.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[EMathOp] =
    withLoc("inst") {
      Decoder.forProduct2[EMathOp, MOp, List[Expr]]("mop", "args")(
        EMathOp.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[EConvert] =
    withLoc("expr") {
      Decoder.forProduct2[EConvert, COp, Expr]("cop", "expr")(EConvert.apply)
    }
  given Decoder[EExists] =
    withLoc("expr") {
      Decoder.forProduct1[EExists, Ref]("ref")(EExists.apply)
    }
  given (using Decoder[Expr]): Decoder[ETypeOf] =
    withLoc("expr") {
      Decoder.forProduct1[ETypeOf, Expr]("base")(ETypeOf.apply)
    }
  given (using Decoder[Expr]): Decoder[EInstanceOf] =
    withLoc("expr") {
      Decoder.forProduct2[EInstanceOf, Expr, Expr]("base", "target")(
        EInstanceOf.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ETypeCheck] =
    withLoc("expr") {
      Decoder.forProduct2[ETypeCheck, Expr, Type]("base", "ty")(
        ETypeCheck.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ESizeOf] =
    withLoc("expr") {
      Decoder.forProduct1[ESizeOf, Expr]("base")(ESizeOf.apply)
    }
  given Decoder[EClo] = withLoc("expr") {
    Decoder.forProduct2[EClo, String, List[Name]]("fname", "captured")(
      EClo.apply,
    )
  }
  given Decoder[ECont] = withLoc("expr") {
    Decoder.forProduct1[ECont, String]("fname")(ECont.apply)
  }
  given (using Decoder[Expr]): Decoder[EDebug] = withLoc("expr") {
    Decoder.forProduct1[EDebug, Expr]("expr")(EDebug.apply)
  }
  given Decoder[ERandom] =
    withLoc("expr") {
      deriveDecoder[ERandom]
    }
  given (using Decoder[Expr]): Decoder[ESyntactic] =
    withLoc("expr") {
      Decoder
        .forProduct4[ESyntactic, String, List[Boolean], Int, List[
          Option[Expr],
        ]](
          "name",
          "args",
          "rhsIdx",
          "children",
        )(
          ESyntactic.apply,
        )
    }
  given (using Decoder[Expr]): Decoder[ELexical] =
    withLoc("expr") {
      Decoder.forProduct2[ELexical, String, Expr]("name", "expr")(
        ELexical.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[ERecord] =
    withLoc("expr") {
      Decoder.forProduct2[ERecord, String, List[(String, Expr)]](
        "tname",
        "pairs",
      )(
        ERecord.apply,
      )
    }
  given (using Decoder[Expr]): Decoder[EMap] = withLoc("expr") {
    Decoder.forProduct2[EMap, (Type, Type), List[(Expr, Expr)]](
      "ty",
      "pairs",
    )(
      EMap.apply,
    )
  }
  given (using Decoder[Expr]): Decoder[EList] = withLoc("expr") {
    Decoder.forProduct1[EList, List[Expr]]("exprs")(EList.apply)
  }
  given (using Decoder[Expr]): Decoder[ECopy] = withLoc("expr") {
    Decoder.forProduct1[ECopy, Expr]("obj")(ECopy.apply)
  }
  given (using Decoder[Expr]): Decoder[EKeys] = withLoc("expr") {
    Decoder.forProduct2[EKeys, Expr, Boolean]("map", "intSorted")(EKeys.apply)
  }
  given Decoder[EMath] = withLoc("expr") {
    Decoder.forProduct1[EMath, BigDecimal]("n")(EMath.apply)
  }
  given Decoder[EInfinity] =
    withLoc("expr") {
      Decoder.forProduct1[EInfinity, Boolean]("pos")(EInfinity.apply)
    }
  given Decoder[ENumber] =
    withLoc("expr") {
      Decoder.forProduct1[ENumber, Double]("double")(ENumber.apply)
    }
  given Decoder[EBigInt] =
    withLoc("expr") {
      Decoder.forProduct1[EBigInt, scala.math.BigInt]("bigInt")(EBigInt.apply)
    }
  given Decoder[EStr] = withLoc("expr") {
    Decoder.forProduct1[EStr, String]("str")(EStr.apply)
  }
  given Decoder[EBool] = withLoc("expr") {
    Decoder.forProduct1[EBool, Boolean]("b")(EBool.apply)
  }
  given Decoder[EUndef] = withLoc("expr") {
    deriveDecoder[EUndef]
  }
  given Decoder[ENull] = withLoc("expr") {
    deriveDecoder[ENull]
  }
  given Decoder[EEnum] = withLoc("expr") {
    Decoder.forProduct1[EEnum, String]("name")(EEnum.apply)
  }
  given Decoder[ECodeUnit] =
    withLoc("expr") {
      deriveDecoder[ECodeUnit]
    }

  given (using Encoder[Expr]): Encoder[EParse] =
    withLoc("expr")(deriveEncoder[EParse])
  given Encoder[EGrammarSymbol] =
    withLoc("expr")(deriveEncoder[EGrammarSymbol])
  given (using Encoder[Expr]): Encoder[ESourceText] =
    withLoc("expr")(deriveEncoder[ESourceText])
  given Encoder[EYet] =
    withLoc("expr")(deriveEncoder[EYet])
  given (using Encoder[Expr]): Encoder[EContains] =
    withLoc("expr")(deriveEncoder[EContains])
  given (using Encoder[Expr]): Encoder[ESubstring] =
    withLoc("expr")(deriveEncoder[ESubstring])
  given (using Encoder[Expr]): Encoder[ETrim] =
    withLoc("expr")(deriveEncoder[ETrim])
  given (using Encoder[Ref]): Encoder[ERef] =
    withLoc("expr")(deriveEncoder[ERef])
  given (using Encoder[Expr]): Encoder[EUnary] =
    withLoc("expr")(deriveEncoder[EUnary])
  given (using Encoder[Expr]): Encoder[EBinary] =
    withLoc("expr")(deriveEncoder[EBinary])
  given (using Encoder[Expr]): Encoder[EVariadic] =
    withLoc("expr")(deriveEncoder[EVariadic])
  given (using Encoder[Expr]): Encoder[EMathOp] =
    withLoc("expr")(deriveEncoder[EMathOp])
  given (using Encoder[Expr]): Encoder[EConvert] =
    withLoc("expr")(deriveEncoder[EConvert])
  given (using Encoder[Ref]): Encoder[EExists] =
    withLoc("expr")(deriveEncoder[EExists])
  given (using Encoder[Expr]): Encoder[ETypeOf] =
    withLoc("expr")(deriveEncoder[ETypeOf])
  given (using Encoder[Expr]): Encoder[EInstanceOf] =
    withLoc("expr")(deriveEncoder[EInstanceOf])
  given (using Encoder[Expr]): Encoder[ETypeCheck] =
    withLoc("expr")(deriveEncoder[ETypeCheck])
  given (using Encoder[Expr]): Encoder[ESizeOf] =
    withLoc("expr")(deriveEncoder[ESizeOf])
  given Encoder[EClo] =
    withLoc("expr")(deriveEncoder[EClo])
  given Encoder[ECont] =
    withLoc("expr")(deriveEncoder[ECont])
  given (using Encoder[Expr]): Encoder[EDebug] =
    withLoc("expr")(deriveEncoder[EDebug])
  given Encoder[ERandom] =
    withLoc("expr")(deriveEncoder[ERandom])
  given (using Encoder[Expr]): Encoder[ESyntactic] =
    withLoc("expr")(deriveEncoder[ESyntactic])
  given (using Encoder[Expr]): Encoder[ELexical] =
    withLoc("expr")(deriveEncoder[ELexical])
  given (using Encoder[Expr]): Encoder[ERecord] =
    withLoc("expr")(deriveEncoder[ERecord])
  given (using Encoder[Expr]): Encoder[EMap] =
    withLoc("expr")(deriveEncoder[EMap])
  given (using Encoder[Expr]): Encoder[EList] =
    withLoc("expr")(deriveEncoder[EList])
  given (using Encoder[Expr]): Encoder[ECopy] =
    withLoc("expr")(deriveEncoder[ECopy])
  given (using Encoder[Expr]): Encoder[EKeys] =
    withLoc("expr")(deriveEncoder[EKeys])
  given Encoder[EMath] =
    withLoc("expr")(deriveEncoder[EMath])
  given Encoder[EInfinity] =
    withLoc("expr")(deriveEncoder[EInfinity])
  given Encoder[ENumber] =
    withLoc("expr")(deriveEncoder[ENumber])
  given Encoder[EBigInt] =
    withLoc("expr")(deriveEncoder[EBigInt])
  given Encoder[EStr] =
    withLoc("expr")(deriveEncoder[EStr])
  given Encoder[EBool] =
    withLoc("expr")(deriveEncoder[EBool])
  given Encoder[EUndef] =
    withLoc("expr")(deriveEncoder[EUndef])
  given Encoder[ENull] =
    withLoc("expr")(deriveEncoder[ENull])
  given Encoder[EEnum] =
    withLoc("expr")(deriveEncoder[EEnum])
  given Encoder[ECodeUnit] =
    withLoc("expr")(deriveEncoder[ECodeUnit])

  /* //////////////////////////////////////////////////////// */

  given Encoder[Param] = encoderWithStringifier(stringify)
  given Decoder[Param] = decoderWithParser(Param.from)
  given (using Decoder[Inst]): Encoder[Func] = deriveEncoder
  given (using Decoder[Inst]): Decoder[Func] = deriveDecoder

  given Encoder[FuncKind] = deriveEncoder
  given Decoder[FuncKind] = deriveDecoder
  given Encoder[Program] = encoderWithStringifier(stringify)
  given Decoder[Program] = decoderWithParser(Program.from)

  // given Encoder[Inst] = deriveEncoder
  // given Decoder[Inst] = deriveDecoder

  given Encoder[Pos] = deriveEncoder
  given Decoder[Pos] = deriveDecoder
  given Encoder[Loc] = deriveEncoder
  given Decoder[Loc] = deriveDecoder

  given Encoder[Inst] =
    withLoc("inst")(deriveEncoder[Inst])
  given Decoder[Inst] =
    withLoc("inst")(deriveDecoder[Inst])

  given Encoder[UOp] = encoderWithStringifier(stringify)
  given Decoder[UOp] = decoderWithParser(UOp.from)
  given Encoder[BOp] = encoderWithStringifier(stringify)
  given Decoder[BOp] = decoderWithParser(BOp.from)
  given Encoder[VOp] = encoderWithStringifier(stringify)
  given Decoder[VOp] = decoderWithParser(VOp.from)
  given Encoder[MOp] = encoderWithStringifier(stringify)
  given Decoder[MOp] = decoderWithParser(MOp.from)
  given Encoder[COp] = encoderWithStringifier(stringify)
  given Decoder[COp] = decoderWithParser(COp.from)

  private transparent inline def withLoc[T <: IRElem with LangEdge](
    // this name should be "unique", so we modify
    name: String,
  )(rule: Encoder[T]): Encoder[T] =
    Encoder.instance { t =>
      Json.obj(
        LANG_OPT_LOC -> (
          for {
            loc <- t.langOpt
            langOptLoc <- loc.loc
          } yield langOptLoc
        ).asJson,
        s"${name}__caseclassobject" -> rule(t),
      )
    }

  private transparent inline def withLoc[T <: IRElem with LangEdge](
    name: String,
  )(rule: Decoder[T]): Decoder[T] =
    Decoder.instance { cursor =>
      for
        t <- cursor.get[T](s"${name}__caseclassobject")(using rule)
        langOptLoc <- cursor.get[Option[Loc]](LANG_OPT_LOC)
      yield {
        t.setLang(new Syntax { loc = langOptLoc })
      }
    }
}
