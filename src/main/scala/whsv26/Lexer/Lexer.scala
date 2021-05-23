package whsv26.Lexer

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject}
import io.circe.parser.parse
import sys.process._
import Token.{PhpToken, SimpleToken, ComplexToken, PhpTokenAttributes, toPhpSimpleToken, toPhpComplexToken}

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
            toPhpSimpleToken(value).map((t: SimpleToken) => PhpToken(t, PhpTokenAttributes(SimpleToken.toString)))
          def onArray(value: Vector[Json]): Option[PhpToken] =
            val tuple = value.toList match
              case token :: content :: line :: Nil => (token.asString, content.asString, line.asNumber)
              case _ => (None, None, None)
            for {
              token      <- tuple._1
              token      <- toPhpComplexToken(token)
              content    <- tuple._2
              line       <- tuple._3
              line       <- line.toInt
              attributes  = PhpTokenAttributes(content, line, line)
            } yield (PhpToken(token, attributes))

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

  def tokenize(path: String)(using d: Decoder[List[PhpToken]]) = {
    for {
      j <- parse(s"""php ${path}""".!!)
      d <- d(j.hcursor)
    } yield (d)
  }
