package whsv26.Parser

import whsv26.Lexer.Token.PhpToken
import whsv26.Parser.Parser.{ExprAdd, Node}
import whsv26.Parser.Parser.AstParser.{expr, rep}

import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.Reader


object Parser:
  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  // Expressions
  case class ExprBool(r: Boolean) extends Expr
  case class ExprInt(r: Int) extends Expr
  case class ExprFloat(r: Float) extends Expr
  case class ExprString(r: Float) extends Expr

  case class ExprAdd(lhs: Expr, rhs: Expr) extends Expr
  case class ExprSub(lhs: Expr, rhs: Expr) extends Expr
  case class ExprMul(lhs: Expr, rhs: Expr) extends Expr
  case class ExprDiv(lhs: Expr, rhs: Expr) extends Expr

  // Statements
  case class Ast(nodes: List[Node]) extends Stmt
  case class StmtEcho(exprs: List[Expr]) extends Stmt

  val code: String = """<?php echo 1.1 - 2 + true * 5; """

  trait ExpressionParser extends RegexParsers:
    def expr: Parser[Expr] = "(" ~> exprBin <~ ")" | exprBin | exprNonBin
    def exprBin = exprSum
    def exprNonBin = exprScalar

    def exprScalar = exprBool | exprFloat| exprInt
    def exprBool = ("true" | "false") ^^ { (s: String) => ExprBool(s == "true")}
    def exprInt = "[0-9]+".r ^^ { (s) => ExprInt(s.toInt)}
    def exprFloat = """[0-9]+\.[0-9]*""".r ^^ { (s) => ExprFloat(s.toFloat)}

    def exprSum = chainl1(exprMul, "+" ^^^ { ExprAdd(_, _) } | "-" ^^^ { ExprSub(_, _) })
    def exprMul = chainl1(exprNonBin, "*" ^^^ { ExprMul(_, _) })

  trait StatementParser extends ExpressionParser:
    def stmt: Parser[Stmt] = stmtEcho
    def stmtEcho: Parser[StmtEcho] = "echo" ~> rep(expr) <~ ";" ^^ { case exprs => StmtEcho(exprs) }

  object AstParser extends RegexParsers with StatementParser:
    def ast: Parser[Ast] = "<?php" ~> rep(stmt) <~ opt("?>") ^^ { case stmts => Ast(stmts)}
    def codeblock: Parser[List[Stmt]] = "{" ~> rep(stmt) <~ "}"




  val parsed = AstParser.parseAll[Ast](AstParser.ast, code)

