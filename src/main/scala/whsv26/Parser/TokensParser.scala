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

  def ast = T_OPEN_TAG ~> rep(stmt) <~ opt(T_CLOSE_TAG) ^^ { case stmts => Ast(stmts)}
  def stmt = echo
  def expr = operator | scalar
  def echo = T_ECHO ~> repsep(expr, T_COMMA) <~ T_SEMICOLON ^^ { case exprs => StmtEcho(exprs) }

  def scalar = float | int | string | bool
  def int = T_LNUMBER ^^ { (t) => ExprInt(t.content.toInt)}
  def float = T_DNUMBER ^^ { (t) => ExprFloat(t.content.toFloat)}
  def string = T_CONSTANT_ENCAPSED_STRING ^^ { (t) => ExprString(t.content)}
  def bool = ("true" | "false") ^^ { (s) => ExprBool(s == "true")}

  def operator: Parser[Expr] = ternary | binary | unary
  def unary = un1
  def un1 = repN(2, T_EXCL) ~> unp | T_EXCL ~> unp ^^ ExprNot.apply
  def unp: Parser[Expr] = T_OPEN_ROUND_BRACKETS ~> operator <~ T_CLOSE_ROUND_BRACKETS | scalar
  def ternary = tern1
  def tern1 = (ternp <~ T_QUESTION) ~ (ternp <~ T_COLON) ~ ternp ^^ { case c ~ l ~ r => ExprTernCond(c, l, r) }
  def ternp: Parser[Expr] = T_OPEN_ROUND_BRACKETS ~> operator <~ T_CLOSE_ROUND_BRACKETS | binary | unary | scalar
  def binary = bin1
  def bin1 = chainl1(bin2,
    T_ASSIGN ^^^ ExprAssign.apply)
  def bin2 = chainl1(bin3,
    T_BOOLEAN_OR ^^^ ExprOr.apply)
  def bin3 = chainl1(bin4,
    T_BOOLEAN_AND ^^^ ExprAnd.apply)
  def bin4 = chainl1(bin5,
    T_IS_NOT_IDENTICAL ^^^ ExprNotEqualStrict.apply |
    T_IS_NOT_EQUAL ^^^ ExprNotEqual.apply |
    T_IS_IDENTICAL ^^^ ExprEqualStrict.apply |
    T_IS_EQUAL ^^^ ExprEqual.apply)
  def bin5 = chainl1(bin6,
    T_IS_SMALLER_OR_EQUAL ^^^ ExprLte.apply |
    T_IS_SMALLER ^^^ ExprLt.apply |
    T_IS_GREATER_OR_EQUAL ^^^ ExprGte.apply |
    T_IS_GREATER ^^^ ExprGt.apply)
  def bin6 = chainl1(bin7,
    T_PLUS ^^^ ExprAdd.apply |
    T_MINUS ^^^ ExprSub.apply)
  def bin7 = chainl1(binp,
    T_ASTERISK ^^^ ExprMul.apply |
    T_SLASH ^^^ ExprDiv.apply |
    T_PERCENT ^^^ ExprMod.apply)
  def binp: Parser[Expr] = T_OPEN_ROUND_BRACKETS ~> operator <~ T_CLOSE_ROUND_BRACKETS | unary | scalar

  def parse(in: Input) = (ast)(in)
