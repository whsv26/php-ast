package whsv26.Parser

object Node:
  sealed trait Node

  object Statement:
    import Expression.Expr
    sealed trait Stmt extends Node
    case class StmtEcho(exprs: List[Expr]) extends Stmt

  object Expression:
    sealed trait Expr extends Node
    case class ExprRaw(r: String) extends Expr

    sealed trait ExprScalar extends Expr
    case class ExprBool(r: Boolean) extends ExprScalar
    case class ExprInt(r: Int) extends ExprScalar
    case class ExprFloat(r: Float) extends ExprScalar
    case class ExprString(r: String) extends ExprScalar

    sealed trait ExprBinary extends Expr
    case class ExprTernCond(cond: Expr, lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprAssign(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprAnd(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprOr(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprNotEqualStrict(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprNotEqual(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprEqualStrict(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprEqual(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprLte(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprLt(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprGte(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprGt(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprAdd(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprSub(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprMul(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprDiv(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprMod(lhs: Expr, rhs: Expr) extends ExprBinary
    case class ExprNot(e: Expr) extends ExprBinary
