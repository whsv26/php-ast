import whsv26.Parser.Parser
import whsv26.Parser.TokensParser
import whsv26.Parser.TokensParser.{PhpTokenReader}
import whsv26.Lexer.Token.{PhpToken, ComplexToken}
import whsv26.Lexer.Lexer
import whsv26.Lexer.Lexer.{tokenize}
import Lexer.Codec.{given, *}
import cats.Show
import cats.implicits.*

object PhpAst {
  def main(args: Array[String]) = {
    val path = "/home/whsv26/Documents/Projects/php-ast/src/main/php/tokens.php"

    val tokens: Either[Throwable, List[PhpToken]] = Lexer.tokenize(path)
    val ts = tokens.getOrElse(Nil)
    val fts = ts.filterNot(_.t == ComplexToken.T_WHITESPACE);

    println(fts.map(_.show).reduceOption(_ + "," +  _).getOrElse(""))
    println()

    val parsed = TokensParser.parse(PhpTokenReader(fts))

    println(parsed)
  }
}
