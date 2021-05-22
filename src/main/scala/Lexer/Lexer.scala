package Lexer

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject}
import io.circe.parser.parse
import sys.process._
import Token.*

object Lexer:
  object Codec:
    given Decoder[PhpToken] with
      def apply(c: HCursor): Decoder.Result[PhpToken] = {
        val j = c.value
        val folder = new Json.Folder[Option[PhpToken]]:
          def onNull: Option[PhpToken] = None
          def onBoolean(value: Boolean): Option[PhpToken] = None
          def onNumber(value: JsonNumber): Option[PhpToken] = None
          def onObject(value: JsonObject): Option[PhpToken] = None
          def onString(value: String): Option[PhpToken] =
            Token.toPhpSimpleToken(value).map((t: PhpSimpleToken) => PhpToken(t, None, None))
          def onArray(value: Vector[Json]): Option[PhpToken] = for {
            tokenJson      <- value.headOption
            tokenString    <- tokenJson.asString
            tokenEnm       <- Token.toPhpComplexToken(tokenString)
            contentJson    <- value.tail.headOption
            contentString  <- contentJson.asString
            lineJson       <- value.tail.tail.headOption
            lineJsonNumber <- lineJson.asNumber
            lineInt        <- lineJsonNumber.toInt
          } yield (PhpToken(tokenEnm, Some(contentString), Some(lineInt)))

        j.foldWith(folder).toRight(DecodingFailure("unable to parse token", j.hcursor.history))
      }

    given (using d: Decoder[PhpToken]): Decoder[List[PhpToken]] with
      def apply(c: HCursor): Decoder.Result[List[PhpToken]] = {
        val values = c.values.getOrElse(Nil).toList
        val listOfDecoded: List[Decoder.Result[PhpToken]] = values.map((j: Json) => d(j.hcursor))

        listOfDecoded.partitionMap(identity) match {
          case (Nil, rights) => Right(rights)
          case (lefts, _) => Left(DecodingFailure("unable to parse token list", c.history))
        }
      }

  import Codec.{given, *}

  def parseTokens(path: String)(using d: Decoder[List[PhpToken]]) = {
    for {
      j <- parse(s"""php ${path}""".!!)
      d <- d(j.hcursor)
    } yield (d)
  }
