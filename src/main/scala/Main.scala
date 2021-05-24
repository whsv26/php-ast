import whsv26.Parser.Parser
import whsv26.Parser.TokensParser
import whsv26.Parser.TokensParser.{PhpTokenReader}
import whsv26.Lexer.Token.{PhpToken, ComplexToken}
import whsv26.Lexer.Lexer
import whsv26.Lexer.Lexer.{tokenize}
import Lexer.Codec.{given, *}

@main def circe(): Unit = {

  val path = "/home/whsv26/Documents/Projects/php-ast/src/main/php/tokens.php"

  val tokens: Either[Throwable, List[PhpToken]] = Lexer.tokenize(path)
  val ts = tokens.getOrElse(Nil)
  println(ts)
//  val ts1 = PhpToken(ComplexToken.T_STRING, "true") :: Nil
  val parsed = TokensParser.parse(PhpTokenReader(ts))

  println(parsed)

//  val path = "/home/whsv26/Documents/Projects/php-ast/src/main/php/parsed.php"
//  println(Parser.parse(path))
}
