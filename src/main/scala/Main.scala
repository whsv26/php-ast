import whsv26.Parser.Parser
import whsv26.Lexer.Lexer
import Lexer.Codec.{given, *}

@main def circe(): Unit = {

  val path = "/home/whsv26/Documents/Projects/php-ast/src/main/php/tokens.php"
  println(Lexer.tokenize(path))

//  val path = "/home/whsv26/Documents/Projects/php-ast/src/main/php/parsed.php"
//  println(Parser.parse(path))
}
