import Lexer.Lexer
import Lexer.Codec.{given, *}

@main def circe(): Unit = {

  val path = "/home/whsv26/Documents/Projects/psalmd/src/main/php/tokens.php"
  println(Lexer.parseTokens(path))
}
