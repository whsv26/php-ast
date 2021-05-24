package whsv26.Parser

import scala.language.implicitConversions
import whsv26.Lexer.Token.{ComplexToken, PhpToken, SimpleToken}
import whsv26.Parser.Parser.AstParser.{expr, rep}
import whsv26.Parser.TokensParser.opt
import scala.sys.process._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}
import Node.Node
import Node.Expression.*
import Node.Statement.*

object TokensParser extends TokenAwareParser:
  case class Ast(nodes: List[Node])

  def scalar = float | int | string | bool
  def int = T_LNUMBER ^^ { (t) => ExprInt(t.content.toInt)}
  def float = T_DNUMBER ^^ { (t) => ExprFloat(t.content.toFloat)}
  def string = T_CONSTANT_ENCAPSED_STRING ^^ { (t) => ExprString(t.content)}
  def bool = ("true" | "false") ^^ { (s) => ExprBool(s == "true")}



  def ast = T_OPEN_TAG ~> T_WHITESPACE ~> rep(stmt) <~ opt(T_CLOSE_TAG) ^^ { case stmts => Ast(stmts)}
  def stmt = echo
  def expr = opt(T_WHITESPACE) ~> (scalar | operator) <~ opt(T_WHITESPACE)
  def echo = T_ECHO ~> T_WHITESPACE ~> repsep(expr, T_COMMA) <~ T_SEMICOLON ^^ { case exprs => StmtEcho(exprs) }


  def operator: Parser[Expr] = ternary | binary | unary
  def unary = u1
  def u1 = repN(2, T_EXCL) ~> mpr | T_EXCL ~> mpr ^^ ExprNot.apply
  def ternary = t1
  def t1 = (mpr <~ T_QUESTION) ~ (mpr <~ T_COLON) ~ mpr ^^ { case c ~ l ~ r => ExprTernCond(c, l, r) }
  def binary = b1
  def b1 = chainl1(b2, T_ASSIGN ^^^ ExprAssign.apply)
  def b2 = chainl1(b3, T_BOOLEAN_OR ^^^ ExprOr.apply)
  def b3 = chainl1(b4, T_BOOLEAN_AND ^^^ ExprAnd.apply)
  def b4 = chainl1(b5,
    T_IS_NOT_IDENTICAL ^^^ ExprNotEqualStrict.apply |
    T_IS_NOT_EQUAL  ^^^ ExprNotEqual.apply |
    T_IS_IDENTICAL ^^^ ExprEqualStrict.apply |
    T_IS_EQUAL  ^^^ ExprEqual.apply)
  def b5 = chainl1(b6,
    T_IS_SMALLER_OR_EQUAL ^^^ ExprLte.apply |
    T_IS_SMALLER          ^^^ ExprLt.apply |
    T_IS_GREATER_OR_EQUAL ^^^ ExprGte.apply |
    T_IS_GREATER          ^^^ ExprGt.apply)
  def b6 = chainl1(b7,
    T_PLUS  ^^^ ExprAdd.apply |
    T_MINUS ^^^ ExprSub.apply)
  def b7 = chainl1(mpr,
    T_ASTERISK ^^^ ExprMul.apply |
    T_SLASH    ^^^ ExprDiv.apply |
    T_PERCENT  ^^^ ExprMod.apply)
  def mpr = scalar | T_OPEN_ROUND_BRACKETS ~> operator <~ T_CLOSE_ROUND_BRACKETS





  def parse(in: Input) = (ast)(in)
