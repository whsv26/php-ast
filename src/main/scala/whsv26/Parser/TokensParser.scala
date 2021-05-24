package whsv26.Parser

import scala.language.implicitConversions
import whsv26.Lexer.Token.{ComplexToken, PhpToken, SimpleToken}
import whsv26.Parser.Parser.AstParser.{expr, rep}
import whsv26.Parser.Parser.Expr
import whsv26.Parser.TokensParser.opt

import scala.sys.process._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object TokensParser extends TokenAwareParser:
  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  case class Ast(nodes: List[Node])
  case class StmtEcho(exprs: List[Expr]) extends Stmt

  sealed trait ExprScalar extends Expr
  case class ExprBool(r: Boolean) extends ExprScalar
  case class ExprInt(r: Int) extends ExprScalar
  case class ExprFloat(r: Float) extends ExprScalar
  case class ExprString(r: String) extends ExprScalar

  def scalar = float | int | string | bool
  def int = T_LNUMBER ^^ { (t) => ExprInt(t.content.toInt)}
  def float = T_DNUMBER ^^ { (t) => ExprFloat(t.content.toFloat)}
  def string = T_CONSTANT_ENCAPSED_STRING ^^ { (t) => ExprString(t.content)}
  def bool = ("true" | "false") ^^ { (s) => ExprBool(s == "true")}

  def ast: Parser[Ast] = T_OPEN_TAG ~> T_WHITESPACE ~> rep(stmt) <~ opt(T_CLOSE_TAG) ^^ { case stmts => Ast(stmts)}
  def stmt: Parser[Stmt] = echo
  def expr: Parser[Expr] = opt(T_WHITESPACE) ~> scalar <~ opt(T_WHITESPACE)
  def echo: Parser[StmtEcho] = T_ECHO ~> T_WHITESPACE ~> repsep(expr, T_COMMA) <~ T_SEMICOLON ^^ { case exprs => StmtEcho(exprs) }


  def parse(in: Input) = (ast)(in)
