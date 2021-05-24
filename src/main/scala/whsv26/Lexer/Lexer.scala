package whsv26.Lexer

import cats.instances.option._
import cats.syntax.apply._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json, JsonNumber, JsonObject}
import io.circe.parser.parse
import sys.process._
import Token.{ComplexToken, PhpToken, FilePos, SimpleToken, toPhpComplexToken, toPhpSimpleToken}

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
            toPhpSimpleToken(value).map((t: SimpleToken) => PhpToken(t, t.content))
          def onArray(value: Vector[Json]): Option[PhpToken] =
            val castToken = (_: Json).asString.flatMap(toPhpComplexToken)
            val castContent = (_: Json).asString
            val castLine = (_: Json).asNumber.flatMap(_.toInt)
            val tuple = value.toList match
              case t :: c :: l :: Nil => (castToken(t), castContent(c), castLine(l))
              case _ => (None, None, None)

            tuple.mapN((token, content, line) => PhpToken(token, content))

        j.foldWith(folder).toRight(DecodingFailure("unable to parse token " + j, j.hcursor.history))
      }

    given (using d: Decoder[PhpToken]): Decoder[List[PhpToken]] with
      def apply(c: HCursor): Decoder.Result[List[PhpToken]] =
        val values = c.values.getOrElse(Nil).toList
        val listOfDecoded: List[Decoder.Result[PhpToken]] = values.map((j: Json) => d(j.hcursor))

        listOfDecoded.partitionMap(identity) match
          case (Nil, rights) => Right(rights)
          case (lefts, _) => Left(DecodingFailure(lefts.toString(), c.history))

  import Codec.{given, *}

  def computePositions(tokens: List[PhpToken]) =
    tokens
      .foldLeft((0, 1, Nil))((acc: (Int, Int, List[PhpToken]), t: PhpToken) => {
        val posStart = acc._1
        val posEnd = posStart + t.content.length
        val lineStart = acc._2
        val lineEnd = lineStart + t.content.count(_ == '\n')
        val positionedTokens = acc._3
        val positionedToken = t.copy(pos = FilePos(lineStart, lineEnd, posStart, posEnd))

        (posEnd + 1, lineEnd, positionedToken :: positionedTokens)
      })
      ._3
      .reverse

  def tokenize(path: String)(using decoder: Decoder[List[PhpToken]]) = {
    for {
      parsedJson <- parse(s"""php ${path}""".!!)
      decoded <- decoder(parsedJson.hcursor)
    } yield (computePositions(decoded))
  }
