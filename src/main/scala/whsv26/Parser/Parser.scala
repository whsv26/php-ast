package whsv26.Parser

import sys.process._
import whsv26.Lexer.Token.PhpToken
import whsv26.Parser.Parser.AstParser.{expr, rep}
import whsv26.Parser.Parser.Expr

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
    case class ExprTernCond(cond: Expr, lhs: Expr, rhs: Expr) extends Expr
    case class ExprAnd(lhs: Expr, rhs: Expr) extends Expr
    case class ExprOr(lhs: Expr, rhs: Expr) extends Expr
    case class ExprNotEqualStrict(lhs: Expr, rhs: Expr) extends Expr
    case class ExprNotEqual(lhs: Expr, rhs: Expr) extends Expr
    case class ExprEqualStrict(lhs: Expr, rhs: Expr) extends Expr
    case class ExprEqual(lhs: Expr, rhs: Expr) extends Expr
    case class ExprLte(lhs: Expr, rhs: Expr) extends Expr
    case class ExprLt(lhs: Expr, rhs: Expr) extends Expr
    case class ExprGte(lhs: Expr, rhs: Expr) extends Expr
    case class ExprGt(lhs: Expr, rhs: Expr) extends Expr
    case class ExprAdd(lhs: Expr, rhs: Expr) extends Expr
    case class ExprSub(lhs: Expr, rhs: Expr) extends Expr
    case class ExprMul(lhs: Expr, rhs: Expr) extends Expr
    case class ExprDiv(lhs: Expr, rhs: Expr) extends Expr
    case class ExprMod(lhs: Expr, rhs: Expr) extends Expr

    trait OpsParser extends ScalarParser:
      def ops: Parser[Expr] = tern | bin
      def tern: Parser[Expr] = t1
      def t1: Parser[Expr] = (mpr <~ "?") ~ (mpr <~ ":") ~ mpr ^^ { case c ~ l ~ r => ExprTernCond(c, l, r) }
      def bin: Parser[Expr] = b2
      def b2: Parser[Expr] = chainl1(b3, "||" ^^^ { ExprOr(_, _) })
      def b3: Parser[Expr] = chainl1(b4, "&&" ^^^ { ExprAnd(_, _) })
      def b4: Parser[Expr] = chainl1(b5,
        "!==" ^^^ { ExprNotEqualStrict(_, _) } |
        "!="  ^^^ { ExprNotEqual(_, _) } |
        "===" ^^^ { ExprEqualStrict(_, _) } |
        "=="  ^^^ { ExprEqual(_, _) })
      def b5: Parser[Expr] = chainl1(b6,
        "<="  ^^^ { ExprLte(_, _) } |
        "<"   ^^^ { ExprLt(_, _) } |
        ">="  ^^^ { ExprGte(_, _) } |
        ">"   ^^^ { ExprGt(_, _) })
      def b6: Parser[Expr] = chainl1(b7,
        "+"   ^^^ { ExprAdd(_, _) } |
        "-"   ^^^ { ExprSub(_, _) })
      def b7: Parser[Expr] = chainl1(mpr,
        "*"   ^^^ { ExprMul(_, _) } |
        "/"   ^^^ { ExprDiv(_, _) } |
        "%"   ^^^ { ExprMod(_, _) })
      def mpr = scalar | "(" ~> ops <~ ")"

  import BinOps.*

  trait ExpressionParser extends OpsParser:
    def expr: Parser[Expr] = ops | scalar

  trait StatementParser extends ExpressionParser:
    def stmt: Parser[Stmt] = echo | """.*\s""".r ^^ { StmtRaw(_) }
    def echo: Parser[StmtEcho] = "echo" ~> repsep(expr, ",") <~ ";" ^^ { case exprs => StmtEcho(exprs) }

  object AstParser extends RegexParsers with StatementParser:
    def ast: Parser[Ast] = "<?php" ~> rep(stmt) <~ opt("?>") ^^ { case stmts => Ast(stmts)}
    def codeblock: Parser[List[Stmt]] = "{" ~> rep(stmt) <~ "}"


  def parse(path: String) = AstParser.parseAll[Ast](AstParser.ast, s"""cat ${path}""".!!)
