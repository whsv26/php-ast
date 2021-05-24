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
  def unary: Parser[Expr] = u1
  def u1: Parser[Expr] = repN(2, T_EXCL) ~> mpr | T_EXCL ~> mpr ^^ ExprNot.apply
  def ternary: Parser[Expr] = t1
  def t1: Parser[Expr] = (mpr <~ "?") ~ (mpr <~ ":") ~ mpr ^^ { case c ~ l ~ r => ExprTernCond(c, l, r) }
  def binary: Parser[Expr] = b1
  def b1: Parser[Expr] = chainl1(b2, "=" ^^^ ExprAssign.apply)
  def b2: Parser[Expr] = chainl1(b3, "||" ^^^ ExprOr.apply)
  def b3: Parser[Expr] = chainl1(b4, "&&" ^^^ ExprAnd.apply)
  def b4: Parser[Expr] = chainl1(b5,
    "!==" ^^^ ExprNotEqualStrict.apply |
    "!="  ^^^ ExprNotEqual.apply |
    "===" ^^^ ExprEqualStrict.apply |
    "=="  ^^^ ExprEqual.apply)
  def b5: Parser[Expr] = chainl1(b6,
    "<="  ^^^ ExprLte.apply |
    "<"   ^^^ ExprLt.apply |
    ">="  ^^^ ExprGte.apply |
    ">"   ^^^ ExprGt.apply)
  def b6: Parser[Expr] = chainl1(b7,
    "+"   ^^^ ExprAdd.apply |
    "-"   ^^^ ExprSub.apply)
  def b7: Parser[Expr] = chainl1(mpr,
    "*"   ^^^ ExprMul.apply |
    "/"   ^^^ ExprDiv.apply |
    "%"   ^^^ ExprMod.apply)
  def mpr: Parser[Expr] = scalar | "(" ~> operator <~ ")"





  def parse(in: Input) = (ast)(in)
