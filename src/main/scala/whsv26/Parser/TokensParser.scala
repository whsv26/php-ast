package whsv26.Parser

import scala.language.implicitConversions
import whsv26.Lexer.Token.{PhpToken, SimpleToken, ComplexToken}
import whsv26.Parser.Parser.AstParser.{expr, rep}
import whsv26.Parser.Parser.Expr
import scala.sys.process._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}


object TokensParser extends Parsers:
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

  object RegCond:
    val int = "[0-9]+?".r
    val float = """[0-9]+\.[0-9]*?""".r
    val string = """('|").*?('|")""".r

  private def acceptToken(token: SimpleToken|ComplexToken) = accept(token.toString, { case t @ PhpToken(_: token.type, _, _) => t })
  private def T_OPEN_ROUND_BRACKETS= acceptToken(SimpleToken.T_OPEN_ROUND_BRACKETS)
  private def T_CLOSE_ROUND_BRACKETS = acceptToken(SimpleToken.T_CLOSE_ROUND_BRACKETS)
  private def T_OPEN_CURLY_BRACKETS = acceptToken(SimpleToken.T_OPEN_CURLY_BRACKETS)
  private def T_CLOSE_CURLY_BRACKETS = acceptToken(SimpleToken.T_CLOSE_CURLY_BRACKETS)
  private def T_OPEN_SQUARE_BRACKETS = acceptToken(SimpleToken.T_OPEN_SQUARE_BRACKETS)
  private def T_CLOSE_SQUARE_BRACKETS = acceptToken(SimpleToken.T_CLOSE_SQUARE_BRACKETS)
  private def T_COMMA = acceptToken(SimpleToken.T_COMMA)
  private def T_DOT = acceptToken(SimpleToken.T_DOT)
  private def T_COLON = acceptToken(SimpleToken.T_COLON)
  private def T_SEMICOLON = acceptToken(SimpleToken.T_SEMICOLON)
  private def T_ASSIGN = acceptToken(SimpleToken.T_ASSIGN)
  private def T_QUESTION = acceptToken(SimpleToken.T_QUESTION)
  private def T_ABSTRACT = acceptToken(ComplexToken.T_ABSTRACT)
  private def T_AND_EQUAL = acceptToken(ComplexToken.T_AND_EQUAL)
  private def T_ARRAY = acceptToken(ComplexToken.T_ARRAY)
  private def T_ARRAY_CAST = acceptToken(ComplexToken.T_ARRAY_CAST)
  private def T_AS = acceptToken(ComplexToken.T_AS)
  private def T_BAD_CHARACTER = acceptToken(ComplexToken.T_BAD_CHARACTER)
  private def T_BOOLEAN_AND = acceptToken(ComplexToken.T_BOOLEAN_AND)
  private def T_BOOLEAN_OR = acceptToken(ComplexToken.T_BOOLEAN_OR)
  private def T_BOOL_CAST = acceptToken(ComplexToken.T_BOOL_CAST)
  private def T_BREAK = acceptToken(ComplexToken.T_BREAK)
  private def T_CALLABLE = acceptToken(ComplexToken.T_CALLABLE)
  private def T_CASE = acceptToken(ComplexToken.T_CASE)
  private def T_CATCH = acceptToken(ComplexToken.T_CATCH)
  private def T_CLASS = acceptToken(ComplexToken.T_CLASS)
  private def T_CLASS_C = acceptToken(ComplexToken.T_CLASS_C)
  private def T_CLONE = acceptToken(ComplexToken.T_CLONE)
  private def T_CLOSE_TAG = acceptToken(ComplexToken.T_CLOSE_TAG)
  private def T_COALESCE = acceptToken(ComplexToken.T_COALESCE)
  private def T_COALESCE_EQUAL = acceptToken(ComplexToken.T_COALESCE_EQUAL)
  private def T_COMMENT = acceptToken(ComplexToken.T_COMMENT)
  private def T_CONCAT_EQUAL = acceptToken(ComplexToken.T_CONCAT_EQUAL)
  private def T_CONST = acceptToken(ComplexToken.T_CONST)
  private def T_CONSTANT_ENCAPSED_STRING = acceptToken(ComplexToken.T_CONSTANT_ENCAPSED_STRING)
  private def T_CONTINUE = acceptToken(ComplexToken.T_CONTINUE)
  private def T_CURLY_OPEN = acceptToken(ComplexToken.T_CURLY_OPEN)
  private def T_DEC = acceptToken(ComplexToken.T_DEC)
  private def T_DECLARE = acceptToken(ComplexToken.T_DECLARE)
  private def T_DEFAULT = acceptToken(ComplexToken.T_DEFAULT)
  private def T_DIR = acceptToken(ComplexToken.T_DIR)
  private def T_DIV_EQUAL = acceptToken(ComplexToken.T_DIV_EQUAL)
  private def T_DNUMBER = acceptToken(ComplexToken.T_DNUMBER)
  private def T_DO = acceptToken(ComplexToken.T_DO)
  private def T_DOC_COMMENT = acceptToken(ComplexToken.T_DOC_COMMENT)
  private def T_DOLLAR_OPEN_CURLY_BRACES = acceptToken(ComplexToken.T_DOLLAR_OPEN_CURLY_BRACES)
  private def T_DOUBLE_ARROW = acceptToken(ComplexToken.T_DOUBLE_ARROW)
  private def T_DOUBLE_CAST = acceptToken(ComplexToken.T_DOUBLE_CAST)
  private def T_DOUBLE_COLON = acceptToken(ComplexToken.T_DOUBLE_COLON)
  private def T_ECHO = acceptToken(ComplexToken.T_ECHO)
  private def T_ELLIPSIS = acceptToken(ComplexToken.T_ELLIPSIS)
  private def T_ELSE = acceptToken(ComplexToken.T_ELSE)
  private def T_ELSEIF = acceptToken(ComplexToken.T_ELSEIF)
  private def T_EMPTY = acceptToken(ComplexToken.T_EMPTY)
  private def T_ENCAPSED_AND_WHITESPACE = acceptToken(ComplexToken.T_ENCAPSED_AND_WHITESPACE)
  private def T_ENDDECLARE = acceptToken(ComplexToken.T_ENDDECLARE)
  private def T_ENDFOR = acceptToken(ComplexToken.T_ENDFOR)
  private def T_ENDFOREACH = acceptToken(ComplexToken.T_ENDFOREACH)
  private def T_ENDIF = acceptToken(ComplexToken.T_ENDIF)
  private def T_ENDSWITCH = acceptToken(ComplexToken.T_ENDSWITCH)
  private def T_ENDWHILE = acceptToken(ComplexToken.T_ENDWHILE)
  private def T_END_HEREDOC = acceptToken(ComplexToken.T_END_HEREDOC)
  private def T_EVAL = acceptToken(ComplexToken.T_EVAL)
  private def T_EXIT = acceptToken(ComplexToken.T_EXIT)
  private def T_EXTENDS = acceptToken(ComplexToken.T_EXTENDS)
  private def T_FILE = acceptToken(ComplexToken.T_FILE)
  private def T_FINAL = acceptToken(ComplexToken.T_FINAL)
  private def T_FINALLY = acceptToken(ComplexToken.T_FINALLY)
  private def T_FN = acceptToken(ComplexToken.T_FN)
  private def T_FOR = acceptToken(ComplexToken.T_FOR)
  private def T_FOREACH = acceptToken(ComplexToken.T_FOREACH)
  private def T_FUNCTION = acceptToken(ComplexToken.T_FUNCTION)
  private def T_FUNC_C = acceptToken(ComplexToken.T_FUNC_C)
  private def T_GLOBAL = acceptToken(ComplexToken.T_GLOBAL)
  private def T_GOTO = acceptToken(ComplexToken.T_GOTO)
  private def T_HALT_COMPILER = acceptToken(ComplexToken.T_HALT_COMPILER)
  private def T_IF = acceptToken(ComplexToken.T_IF)
  private def T_IMPLEMENTS = acceptToken(ComplexToken.T_IMPLEMENTS)
  private def T_INC = acceptToken(ComplexToken.T_INC)
  private def T_INCLUDE = acceptToken(ComplexToken.T_INCLUDE)
  private def T_INCLUDE_ONCE = acceptToken(ComplexToken.T_INCLUDE_ONCE)
  private def T_INLINE_HTML = acceptToken(ComplexToken.T_INLINE_HTML)
  private def T_INSTANCEOF = acceptToken(ComplexToken.T_INSTANCEOF)
  private def T_INSTEADOF = acceptToken(ComplexToken.T_INSTEADOF)
  private def T_INTERFACE = acceptToken(ComplexToken.T_INTERFACE)
  private def T_INT_CAST = acceptToken(ComplexToken.T_INT_CAST)
  private def T_ISSET = acceptToken(ComplexToken.T_ISSET)
  private def T_IS_EQUAL = acceptToken(ComplexToken.T_IS_EQUAL)
  private def T_IS_GREATER_OR_EQUAL = acceptToken(ComplexToken.T_IS_GREATER_OR_EQUAL)
  private def T_IS_IDENTICAL = acceptToken(ComplexToken.T_IS_IDENTICAL)
  private def T_IS_NOT_EQUAL = acceptToken(ComplexToken.T_IS_NOT_EQUAL)
  private def T_IS_NOT_IDENTICAL = acceptToken(ComplexToken.T_IS_NOT_IDENTICAL)
  private def T_IS_SMALLER_OR_EQUAL = acceptToken(ComplexToken.T_IS_SMALLER_OR_EQUAL)
  private def T_LINE = acceptToken(ComplexToken.T_LINE)
  private def T_LIST = acceptToken(ComplexToken.T_LIST)
  private def T_LNUMBER = acceptToken(ComplexToken.T_LNUMBER)
  private def T_LOGICAL_AND = acceptToken(ComplexToken.T_LOGICAL_AND)
  private def T_LOGICAL_OR = acceptToken(ComplexToken.T_LOGICAL_OR)
  private def T_LOGICAL_XOR = acceptToken(ComplexToken.T_LOGICAL_XOR)
  private def T_METHOD_C = acceptToken(ComplexToken.T_METHOD_C)
  private def T_MINUS_EQUAL = acceptToken(ComplexToken.T_MINUS_EQUAL)
  private def T_MOD_EQUAL = acceptToken(ComplexToken.T_MOD_EQUAL)
  private def T_MUL_EQUAL = acceptToken(ComplexToken.T_MUL_EQUAL)
  private def T_NAMESPACE = acceptToken(ComplexToken.T_NAMESPACE)
  private def T_NEW = acceptToken(ComplexToken.T_NEW)
  private def T_NS_C = acceptToken(ComplexToken.T_NS_C)
  private def T_NS_SEPARATOR = acceptToken(ComplexToken.T_NS_SEPARATOR)
  private def T_NUM_STRING = acceptToken(ComplexToken.T_NUM_STRING)
  private def T_OBJECT_CAST = acceptToken(ComplexToken.T_OBJECT_CAST)
  private def T_OBJECT_OPERATOR = acceptToken(ComplexToken.T_OBJECT_OPERATOR)
  private def T_NULLSAFE_OBJECT_OPERATOR = acceptToken(ComplexToken.T_NULLSAFE_OBJECT_OPERATOR)
  private def T_OPEN_TAG = acceptToken(ComplexToken.T_OPEN_TAG)
  private def T_OPEN_TAG_WITH_ECHO = acceptToken(ComplexToken.T_OPEN_TAG_WITH_ECHO)
  private def T_OR_EQUAL = acceptToken(ComplexToken.T_OR_EQUAL)
  private def T_PAAMAYIM_NEKUDOTAYIM = acceptToken(ComplexToken.T_PAAMAYIM_NEKUDOTAYIM)
  private def T_PLUS_EQUAL = acceptToken(ComplexToken.T_PLUS_EQUAL)
  private def T_POW = acceptToken(ComplexToken.T_POW)
  private def T_POW_EQUAL = acceptToken(ComplexToken.T_POW_EQUAL)
  private def T_PRINT = acceptToken(ComplexToken.T_PRINT)
  private def T_PRIVATE = acceptToken(ComplexToken.T_PRIVATE)
  private def T_PROTECTED = acceptToken(ComplexToken.T_PROTECTED)
  private def T_PUBLIC = acceptToken(ComplexToken.T_PUBLIC)
  private def T_REQUIRE = acceptToken(ComplexToken.T_REQUIRE)
  private def T_REQUIRE_ONCE = acceptToken(ComplexToken.T_REQUIRE_ONCE)
  private def T_RETURN = acceptToken(ComplexToken.T_RETURN)
  private def T_SL = acceptToken(ComplexToken.T_SL)
  private def T_SL_EQUAL = acceptToken(ComplexToken.T_SL_EQUAL)
  private def T_SPACESHIP = acceptToken(ComplexToken.T_SPACESHIP)
  private def T_SR = acceptToken(ComplexToken.T_SR)
  private def T_SR_EQUAL = acceptToken(ComplexToken.T_SR_EQUAL)
  private def T_START_HEREDOC = acceptToken(ComplexToken.T_START_HEREDOC)
  private def T_STATIC = acceptToken(ComplexToken.T_STATIC)
  private def T_STRING = acceptToken(ComplexToken.T_STRING)
  private def T_STRING_CAST = acceptToken(ComplexToken.T_STRING_CAST)
  private def T_STRING_VARNAME = acceptToken(ComplexToken.T_STRING_VARNAME)
  private def T_SWITCH = acceptToken(ComplexToken.T_SWITCH)
  private def T_THROW = acceptToken(ComplexToken.T_THROW)
  private def T_TRAIT = acceptToken(ComplexToken.T_TRAIT)
  private def T_TRAIT_C = acceptToken(ComplexToken.T_TRAIT_C)
  private def T_TRY = acceptToken(ComplexToken.T_TRY)
  private def T_UNSET = acceptToken(ComplexToken.T_UNSET)
  private def T_UNSET_CAST = acceptToken(ComplexToken.T_UNSET_CAST)
  private def T_USE = acceptToken(ComplexToken.T_USE)
  private def T_VAR = acceptToken(ComplexToken.T_VAR)
  private def T_VARIABLE = acceptToken(ComplexToken.T_VARIABLE)
  private def T_WHILE = acceptToken(ComplexToken.T_WHILE)
  private def T_WHITESPACE = acceptToken(ComplexToken.T_WHITESPACE)
  private def T_XOR_EQUAL = acceptToken(ComplexToken.T_XOR_EQUAL)
  private def T_YIELD = acceptToken(ComplexToken.T_YIELD)
  private def T_YIELD_FROM = acceptToken(ComplexToken.T_YIELD_FROM)

  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  // Statements
  case class Ast(nodes: List[Node]) extends Node
  case class StmtEcho(exprs: List[Expr]) extends Stmt

  given Conversion[String, Parser[String]] with
    def apply(s: String) = new Parser[String] {
      def apply(in: Input) = {
        if (!in.atEnd && in.first.content == s)
          Success(s, in.rest)
        else  {
          Failure("'"+s+"' expected", in.rest)
        }
      }
    }

  case class ExprBool(r: Boolean) extends Expr
  case class ExprInt(r: Int) extends Expr
  case class ExprFloat(r: Float) extends Expr
  case class ExprString(r: String) extends Expr

  def scalar: Parser[Expr] = bool | float | int | string
  def bool: Parser[ExprBool] = ("true" | "false") ^^ { (s) => ExprBool(s == "true")}
  def int: Parser[ExprInt] = T_LNUMBER ^^ { (t) => ExprInt(t.content.toInt)}
  def float: Parser[ExprFloat] = T_DNUMBER ^^ { (t) => ExprFloat(t.content.toFloat)}
  def string: Parser[ExprString] = T_CONSTANT_ENCAPSED_STRING ^^ { (t) => ExprString(t.content)}



//  def expr: Parser[Expr] = ops | scalar
//  def stmt: Parser[Stmt] = echo | """.*\s""".r ^^ { StmtRaw(_) }
//  def echo: Parser[StmtEcho] = "echo" ~> repsep(expr, ",") <~ ";" ^^ { case exprs => StmtEcho(exprs) }
//  def ast: Parser[Ast] = "<?php" ~> rep(stmt) <~ opt("?>") ^^ { case stmts => Ast(stmts)}
//  def codeblock: Parser[List[Stmt]] = "{" ~> rep(stmt) <~ "}"

  def parse(in: Input) = (scalar)(in)
