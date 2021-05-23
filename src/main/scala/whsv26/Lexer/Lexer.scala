package whsv26.Lexer

import cats.instances.option._
import cats.syntax.apply._
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
            val castToken = (_: Json).asString.flatMap(toPhpComplexToken)
            val castContent = (_: Json).asString
            val castLine = (_: Json).asNumber.flatMap(_.toInt)
            val tuple = value.toList match
              case t :: c :: l :: Nil => (castToken(t), castContent(c), castLine(l))
              case _ => (None, None, None)

            tuple.mapN((token, content, line) => PhpToken(token, PhpTokenAttributes(content, line, line)))

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
