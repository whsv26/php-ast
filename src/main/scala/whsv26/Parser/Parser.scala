package whsv26.Parser

import whsv26.Lexer.Token.PhpToken
import whsv26.Parser.Parser.Node

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.Reader


object Parser:
  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  // Expressions
  case class ExprBool(val r: Boolean) extends Expr

  // Statements
  case class StmtPhp(nodes: List[Node]) extends Node
  case class StmtEcho(exprs: List[Expr]) extends Stmt

  val code: String = """<?php echo true; """


  object PhpParser extends RegexParsers:
    def php: Parser[StmtPhp] = "<?php" ~> codeblock <~ opt("?>") ^^ { case stmts => StmtPhp(stmts)}
    def codeblock: Parser[List[Stmt]] = rep(stmt)
    def stmt: Parser[Stmt] = stmtEcho
    def stmtEcho: Parser[StmtEcho] = "echo" ~> rep(expr) <~ ";" ^^ { case exprs => StmtEcho(exprs) }
    def expr: Parser[Expr] = exprBool
    def exprBool: Parser[ExprBool] = ("true" | "false") ^^ { (s: String) => ExprBool(s == "true")}


  val parsed = PhpParser.parseAll[StmtPhp](PhpParser.php, code)

