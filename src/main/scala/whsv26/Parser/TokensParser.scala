package whsv26.Parser

import scala.language.implicitConversions
import whsv26.Lexer.Token.{PhpToken, SimpleToken, ComplexToken}
import whsv26.Parser.Parser.AstParser.{expr, rep}
import whsv26.Parser.Parser.Expr
import scala.sys.process._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

object TokensParser extends TokenAwareParser:
  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  // Statements
  case class Ast(nodes: List[Node]) extends Node
  case class StmtEcho(exprs: List[Expr]) extends Stmt

  case class ExprBool(r: Boolean) extends Expr
  case class ExprInt(r: Int) extends Expr
  case class ExprFloat(r: Float) extends Expr
  case class ExprString(r: String) extends Expr

  def scalar = bool | float | int | string
  def bool = ("true" | "false") ^^ { (s) => ExprBool(s == "true")}
  def int = T_LNUMBER ^^ { (t) => ExprInt(t.content.toInt)}
  def float = T_DNUMBER ^^ { (t) => ExprFloat(t.content.toFloat)}
  def string = T_CONSTANT_ENCAPSED_STRING ^^ { (t) => ExprString(t.content)}



//  def expr: Parser[Expr] = ops | scalar
//  def stmt: Parser[Stmt] = echo | """.*\s""".r ^^ { StmtRaw(_) }
//  def echo: Parser[StmtEcho] = "echo" ~> repsep(expr, ",") <~ ";" ^^ { case exprs => StmtEcho(exprs) }
//  def ast: Parser[Ast] = "<?php" ~> rep(stmt) <~ opt("?>") ^^ { case stmts => Ast(stmts)}
//  def codeblock: Parser[List[Stmt]] = "{" ~> rep(stmt) <~ "}"

  def parse(in: Input) = (scalar)(in)
