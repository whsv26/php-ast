package whsv26.Parser

import sys.process._
import whsv26.Lexer.Token.PhpToken
import whsv26.Parser.Parser.AstParser.{expr, rep}
import scala.util.matching.Regex
import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.Reader


object Parser:
  sealed trait Node
  sealed trait Stmt extends Node
  sealed trait Expr extends Node

  // Expressions
  case class ExprRaw(r: String) extends Expr

  // Statements
  case class Ast(nodes: List[Node]) extends Stmt
  case class StmtEcho(exprs: List[Expr]) extends Stmt
  case class StmtRaw(r: String) extends Stmt

  object RegCond:
    val int = "[0-9]+".r
    val float = """[0-9]+\.[0-9]*""".r
    val string = """('|").*?('|")""".r

  object Scalar:
    case class ExprBool(r: Boolean) extends Expr
    case class ExprInt(r: Int) extends Expr
    case class ExprFloat(r: Float) extends Expr
    case class ExprString(r: String) extends Expr

    trait ScalarParser  extends RegexParsers:
      def scalar = bool | float | int | string
      def bool = ("true" | "false") ^^ { (s: String) => ExprBool(s == "true")}
      def int = RegCond.int ^^ { (s) => ExprInt(s.toInt)}
      def float = RegCond.float ^^ { (s) => ExprFloat(s.toFloat)}
      def string = RegCond.string ^^ { (s) => ExprString(s.toString)}

  object BinOps:
    import Scalar.*
    case class ExprEqual(lhs: Expr, rhs: Expr) extends Expr
    case class ExprEqualStrict(lhs: Expr, rhs: Expr) extends Expr
    case class ExprNotEqual(lhs: Expr, rhs: Expr) extends Expr
    case class ExprNotEqualStrict(lhs: Expr, rhs: Expr) extends Expr
    case class ExprAdd(lhs: Expr, rhs: Expr) extends Expr
    case class ExprSub(lhs: Expr, rhs: Expr) extends Expr
    case class ExprMul(lhs: Expr, rhs: Expr) extends Expr
    case class ExprMod(lhs: Expr, rhs: Expr) extends Expr
    case class ExprDiv(lhs: Expr, rhs: Expr) extends Expr

    trait BinOpsParser extends ScalarParser:
      def bin: Parser[Expr] = p1 | p4
      def p1: Parser[Expr] = chainl1(p2,
        "!==" ^^^ { ExprNotEqualStrict(_, _) } |
        "!="  ^^^ { ExprNotEqual(_, _) } |
        "===" ^^^ { ExprEqualStrict(_, _) } |
        "=="  ^^^ { ExprEqual(_, _) })
      def p2: Parser[Expr] = chainl1(p3,
        "+"   ^^^ { ExprAdd(_, _) } |
        "-"   ^^^ { ExprSub(_, _) })
      def p3: Parser[Expr] = chainl1(p4,
        "*"   ^^^ { ExprMul(_, _) } |
        "%"   ^^^ { ExprMod(_, _) })
      def p4 = scalar | "(" ~> bin <~ ")"

  import BinOps.*

  trait ExpressionParser extends BinOpsParser:
    def expr: Parser[Expr] = bin | scalar

  trait StatementParser extends ExpressionParser:
    def stmt: Parser[Stmt] = echo | """.*\s""".r ^^ { StmtRaw(_) }
    def echo: Parser[StmtEcho] = "echo" ~> repsep(expr, ",") <~ ";" ^^ { case exprs => StmtEcho(exprs) }

  object AstParser extends RegexParsers with StatementParser:
    def ast: Parser[Ast] = "<?php" ~> rep(stmt) <~ opt("?>") ^^ { case stmts => Ast(stmts)}
    def codeblock: Parser[List[Stmt]] = "{" ~> rep(stmt) <~ "}"


  def parse(path: String) = AstParser.parseAll[Ast](AstParser.ast, s"""cat ${path}""".!!)
