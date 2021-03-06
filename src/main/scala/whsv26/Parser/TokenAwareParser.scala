package whsv26.Parser

import cats.Show
import whsv26.Lexer.Token.{ComplexToken, PhpToken, SimpleToken}
import whsv26.Parser.Node.Expression.ExprRaw
import whsv26.Parser.Node.Expression.Expr
import whsv26.Parser.TokensParser.{Input, Success, accept}

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait TokenAwareParser extends Parsers:
  override type Elem = PhpToken

  object PhpTokenPosition extends Position: // TODO
    override def line: Int = ???
    override def column: Int = ???
    override protected def lineContents: String = ???

  class PhpTokenReader(tokens: List[PhpToken]) extends Reader[PhpToken]:
    override def first = tokens.head
    override def atEnd = tokens.isEmpty
    override def pos = NoPosition
    override def rest: Reader[PhpToken] = new PhpTokenReader(tokens.tail)

  /**
   * Implicit string to parser conversions
   */
  given Conversion[String, Parser[String]] with
    def apply(s: String) = new Parser[String]:
      def apply(in: Input) = {
        if (!in.atEnd && in.first.content == s)
          Success(s, in.rest)
        else  {
          Failure("'"+s+"' expected", in.rest)
        }
      }

  def debugParser(using s: Show[PhpToken]) = new Parser[String]:
    def apply(in: Input) = {
      val buffer: ListBuffer[PhpToken] = ListBuffer()
      var inp = in

      while (!inp.atEnd) {
        buffer += inp.first
        inp = inp.rest
      }

      val reduced = buffer.map(s.show(_)).reduceOption(_ + "," +  _)
      Success(reduced.getOrElse(""), in.rest)
    }


  def acceptToken(token: SimpleToken|ComplexToken) = accept(token.toString, { case t @ PhpToken(_: token.type, _, _) => t })

  def T_OPEN_ROUND_BRACKETS= acceptToken(SimpleToken.T_OPEN_ROUND_BRACKETS)
  def T_CLOSE_ROUND_BRACKETS = acceptToken(SimpleToken.T_CLOSE_ROUND_BRACKETS)
  def T_OPEN_CURLY_BRACKETS = acceptToken(SimpleToken.T_OPEN_CURLY_BRACKETS)
  def T_CLOSE_CURLY_BRACKETS = acceptToken(SimpleToken.T_CLOSE_CURLY_BRACKETS)
  def T_OPEN_SQUARE_BRACKETS = acceptToken(SimpleToken.T_OPEN_SQUARE_BRACKETS)
  def T_CLOSE_SQUARE_BRACKETS = acceptToken(SimpleToken.T_CLOSE_SQUARE_BRACKETS)
  def T_COMMA = acceptToken(SimpleToken.T_COMMA)
  def T_DOT = acceptToken(SimpleToken.T_DOT)
  def T_COLON = acceptToken(SimpleToken.T_COLON)
  def T_SEMICOLON = acceptToken(SimpleToken.T_SEMICOLON)
  def T_ASSIGN = acceptToken(SimpleToken.T_ASSIGN)
  def T_QUESTION = acceptToken(SimpleToken.T_QUESTION)
  def T_PLUS = acceptToken(SimpleToken.T_PLUS)
  def T_MINUS = acceptToken(SimpleToken.T_MINUS)
  def T_EXCL = acceptToken(SimpleToken.T_EXCL)
  def T_IS_SMALLER = acceptToken(SimpleToken.T_IS_SMALLER)
  def T_IS_GREATER = acceptToken(SimpleToken.T_IS_GREATER)
  def T_ASTERISK = acceptToken(SimpleToken.T_ASTERISK)
  def T_SLASH = acceptToken(SimpleToken.T_SLASH)
  def T_PERCENT = acceptToken(SimpleToken.T_PERCENT)

  def T_ABSTRACT = acceptToken(ComplexToken.T_ABSTRACT)
  def T_AND_EQUAL = acceptToken(ComplexToken.T_AND_EQUAL)
  def T_ARRAY = acceptToken(ComplexToken.T_ARRAY)
  def T_ARRAY_CAST = acceptToken(ComplexToken.T_ARRAY_CAST)
  def T_AS = acceptToken(ComplexToken.T_AS)
  def T_BAD_CHARACTER = acceptToken(ComplexToken.T_BAD_CHARACTER)
  def T_BOOLEAN_AND = acceptToken(ComplexToken.T_BOOLEAN_AND)
  def T_BOOLEAN_OR = acceptToken(ComplexToken.T_BOOLEAN_OR)
  def T_BOOL_CAST = acceptToken(ComplexToken.T_BOOL_CAST)
  def T_BREAK = acceptToken(ComplexToken.T_BREAK)
  def T_CALLABLE = acceptToken(ComplexToken.T_CALLABLE)
  def T_CASE = acceptToken(ComplexToken.T_CASE)
  def T_CATCH = acceptToken(ComplexToken.T_CATCH)
  def T_CLASS = acceptToken(ComplexToken.T_CLASS)
  def T_CLASS_C = acceptToken(ComplexToken.T_CLASS_C)
  def T_CLONE = acceptToken(ComplexToken.T_CLONE)
  def T_CLOSE_TAG = acceptToken(ComplexToken.T_CLOSE_TAG)
  def T_COALESCE = acceptToken(ComplexToken.T_COALESCE)
  def T_COALESCE_EQUAL = acceptToken(ComplexToken.T_COALESCE_EQUAL)
  def T_COMMENT = acceptToken(ComplexToken.T_COMMENT)
  def T_CONCAT_EQUAL = acceptToken(ComplexToken.T_CONCAT_EQUAL)
  def T_CONST = acceptToken(ComplexToken.T_CONST)
  def T_CONSTANT_ENCAPSED_STRING = acceptToken(ComplexToken.T_CONSTANT_ENCAPSED_STRING)
  def T_CONTINUE = acceptToken(ComplexToken.T_CONTINUE)
  def T_CURLY_OPEN = acceptToken(ComplexToken.T_CURLY_OPEN)
  def T_DEC = acceptToken(ComplexToken.T_DEC)
  def T_DECLARE = acceptToken(ComplexToken.T_DECLARE)
  def T_DEFAULT = acceptToken(ComplexToken.T_DEFAULT)
  def T_DIR = acceptToken(ComplexToken.T_DIR)
  def T_DIV_EQUAL = acceptToken(ComplexToken.T_DIV_EQUAL)
  def T_DNUMBER = acceptToken(ComplexToken.T_DNUMBER)
  def T_DO = acceptToken(ComplexToken.T_DO)
  def T_DOC_COMMENT = acceptToken(ComplexToken.T_DOC_COMMENT)
  def T_DOLLAR_OPEN_CURLY_BRACES = acceptToken(ComplexToken.T_DOLLAR_OPEN_CURLY_BRACES)
  def T_DOUBLE_ARROW = acceptToken(ComplexToken.T_DOUBLE_ARROW)
  def T_DOUBLE_CAST = acceptToken(ComplexToken.T_DOUBLE_CAST)
  def T_DOUBLE_COLON = acceptToken(ComplexToken.T_DOUBLE_COLON)
  def T_ECHO = acceptToken(ComplexToken.T_ECHO)
  def T_ELLIPSIS = acceptToken(ComplexToken.T_ELLIPSIS)
  def T_ELSE = acceptToken(ComplexToken.T_ELSE)
  def T_ELSEIF = acceptToken(ComplexToken.T_ELSEIF)
  def T_EMPTY = acceptToken(ComplexToken.T_EMPTY)
  def T_ENCAPSED_AND_WHITESPACE = acceptToken(ComplexToken.T_ENCAPSED_AND_WHITESPACE)
  def T_ENDDECLARE = acceptToken(ComplexToken.T_ENDDECLARE)
  def T_ENDFOR = acceptToken(ComplexToken.T_ENDFOR)
  def T_ENDFOREACH = acceptToken(ComplexToken.T_ENDFOREACH)
  def T_ENDIF = acceptToken(ComplexToken.T_ENDIF)
  def T_ENDSWITCH = acceptToken(ComplexToken.T_ENDSWITCH)
  def T_ENDWHILE = acceptToken(ComplexToken.T_ENDWHILE)
  def T_END_HEREDOC = acceptToken(ComplexToken.T_END_HEREDOC)
  def T_EVAL = acceptToken(ComplexToken.T_EVAL)
  def T_EXIT = acceptToken(ComplexToken.T_EXIT)
  def T_EXTENDS = acceptToken(ComplexToken.T_EXTENDS)
  def T_FILE = acceptToken(ComplexToken.T_FILE)
  def T_FINAL = acceptToken(ComplexToken.T_FINAL)
  def T_FINALLY = acceptToken(ComplexToken.T_FINALLY)
  def T_FN = acceptToken(ComplexToken.T_FN)
  def T_FOR = acceptToken(ComplexToken.T_FOR)
  def T_FOREACH = acceptToken(ComplexToken.T_FOREACH)
  def T_FUNCTION = acceptToken(ComplexToken.T_FUNCTION)
  def T_FUNC_C = acceptToken(ComplexToken.T_FUNC_C)
  def T_GLOBAL = acceptToken(ComplexToken.T_GLOBAL)
  def T_GOTO = acceptToken(ComplexToken.T_GOTO)
  def T_HALT_COMPILER = acceptToken(ComplexToken.T_HALT_COMPILER)
  def T_IF = acceptToken(ComplexToken.T_IF)
  def T_IMPLEMENTS = acceptToken(ComplexToken.T_IMPLEMENTS)
  def T_INC = acceptToken(ComplexToken.T_INC)
  def T_INCLUDE = acceptToken(ComplexToken.T_INCLUDE)
  def T_INCLUDE_ONCE = acceptToken(ComplexToken.T_INCLUDE_ONCE)
  def T_INLINE_HTML = acceptToken(ComplexToken.T_INLINE_HTML)
  def T_INSTANCEOF = acceptToken(ComplexToken.T_INSTANCEOF)
  def T_INSTEADOF = acceptToken(ComplexToken.T_INSTEADOF)
  def T_INTERFACE = acceptToken(ComplexToken.T_INTERFACE)
  def T_INT_CAST = acceptToken(ComplexToken.T_INT_CAST)
  def T_ISSET = acceptToken(ComplexToken.T_ISSET)
  def T_IS_EQUAL = acceptToken(ComplexToken.T_IS_EQUAL)
  def T_IS_GREATER_OR_EQUAL = acceptToken(ComplexToken.T_IS_GREATER_OR_EQUAL)
  def T_IS_IDENTICAL = acceptToken(ComplexToken.T_IS_IDENTICAL)
  def T_IS_NOT_EQUAL = acceptToken(ComplexToken.T_IS_NOT_EQUAL)
  def T_IS_NOT_IDENTICAL = acceptToken(ComplexToken.T_IS_NOT_IDENTICAL)
  def T_IS_SMALLER_OR_EQUAL = acceptToken(ComplexToken.T_IS_SMALLER_OR_EQUAL)
  def T_LINE = acceptToken(ComplexToken.T_LINE)
  def T_LIST = acceptToken(ComplexToken.T_LIST)
  def T_LNUMBER = acceptToken(ComplexToken.T_LNUMBER)
  def T_LOGICAL_AND = acceptToken(ComplexToken.T_LOGICAL_AND)
  def T_LOGICAL_OR = acceptToken(ComplexToken.T_LOGICAL_OR)
  def T_LOGICAL_XOR = acceptToken(ComplexToken.T_LOGICAL_XOR)
  def T_METHOD_C = acceptToken(ComplexToken.T_METHOD_C)
  def T_MINUS_EQUAL = acceptToken(ComplexToken.T_MINUS_EQUAL)
  def T_MOD_EQUAL = acceptToken(ComplexToken.T_MOD_EQUAL)
  def T_MUL_EQUAL = acceptToken(ComplexToken.T_MUL_EQUAL)
  def T_NAMESPACE = acceptToken(ComplexToken.T_NAMESPACE)
  def T_NEW = acceptToken(ComplexToken.T_NEW)
  def T_NS_C = acceptToken(ComplexToken.T_NS_C)
  def T_NS_SEPARATOR = acceptToken(ComplexToken.T_NS_SEPARATOR)
  def T_NUM_STRING = acceptToken(ComplexToken.T_NUM_STRING)
  def T_OBJECT_CAST = acceptToken(ComplexToken.T_OBJECT_CAST)
  def T_OBJECT_OPERATOR = acceptToken(ComplexToken.T_OBJECT_OPERATOR)
  def T_NULLSAFE_OBJECT_OPERATOR = acceptToken(ComplexToken.T_NULLSAFE_OBJECT_OPERATOR)
  def T_OPEN_TAG = acceptToken(ComplexToken.T_OPEN_TAG)
  def T_OPEN_TAG_WITH_ECHO = acceptToken(ComplexToken.T_OPEN_TAG_WITH_ECHO)
  def T_OR_EQUAL = acceptToken(ComplexToken.T_OR_EQUAL)
  def T_PAAMAYIM_NEKUDOTAYIM = acceptToken(ComplexToken.T_PAAMAYIM_NEKUDOTAYIM)
  def T_PLUS_EQUAL = acceptToken(ComplexToken.T_PLUS_EQUAL)
  def T_POW = acceptToken(ComplexToken.T_POW)
  def T_POW_EQUAL = acceptToken(ComplexToken.T_POW_EQUAL)
  def T_PRINT = acceptToken(ComplexToken.T_PRINT)
  def T_PRIVATE = acceptToken(ComplexToken.T_PRIVATE)
  def T_PROTECTED = acceptToken(ComplexToken.T_PROTECTED)
  def T_PUBLIC = acceptToken(ComplexToken.T_PUBLIC)
  def T_REQUIRE = acceptToken(ComplexToken.T_REQUIRE)
  def T_REQUIRE_ONCE = acceptToken(ComplexToken.T_REQUIRE_ONCE)
  def T_RETURN = acceptToken(ComplexToken.T_RETURN)
  def T_SL = acceptToken(ComplexToken.T_SL)
  def T_SL_EQUAL = acceptToken(ComplexToken.T_SL_EQUAL)
  def T_SPACESHIP = acceptToken(ComplexToken.T_SPACESHIP)
  def T_SR = acceptToken(ComplexToken.T_SR)
  def T_SR_EQUAL = acceptToken(ComplexToken.T_SR_EQUAL)
  def T_START_HEREDOC = acceptToken(ComplexToken.T_START_HEREDOC)
  def T_STATIC = acceptToken(ComplexToken.T_STATIC)
  def T_STRING = acceptToken(ComplexToken.T_STRING)
  def T_STRING_CAST = acceptToken(ComplexToken.T_STRING_CAST)
  def T_STRING_VARNAME = acceptToken(ComplexToken.T_STRING_VARNAME)
  def T_SWITCH = acceptToken(ComplexToken.T_SWITCH)
  def T_THROW = acceptToken(ComplexToken.T_THROW)
  def T_TRAIT = acceptToken(ComplexToken.T_TRAIT)
  def T_TRAIT_C = acceptToken(ComplexToken.T_TRAIT_C)
  def T_TRY = acceptToken(ComplexToken.T_TRY)
  def T_UNSET = acceptToken(ComplexToken.T_UNSET)
  def T_UNSET_CAST = acceptToken(ComplexToken.T_UNSET_CAST)
  def T_USE = acceptToken(ComplexToken.T_USE)
  def T_VAR = acceptToken(ComplexToken.T_VAR)
  def T_VARIABLE = acceptToken(ComplexToken.T_VARIABLE)
  def T_WHILE = acceptToken(ComplexToken.T_WHILE)
  def T_WHITESPACE = acceptToken(ComplexToken.T_WHITESPACE)
  def T_XOR_EQUAL = acceptToken(ComplexToken.T_XOR_EQUAL)
  def T_YIELD = acceptToken(ComplexToken.T_YIELD)
  def T_YIELD_FROM = acceptToken(ComplexToken.T_YIELD_FROM)

