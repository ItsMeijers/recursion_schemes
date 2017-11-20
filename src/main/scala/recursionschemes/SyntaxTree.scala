package recursionschemes

private object SyntaxTree {

  sealed trait Lit
  final case class StringLiteral(s: String) extends Lit
  final case class IntLiteral(i: Int) extends Lit
  final case class Identity(s: String) extends Lit

  sealed trait Expr
  final case class Index(expression1: Expr, expression2: Expr) extends Expr
  final case class Call(expression: Expr, arguments: List[Expr]) extends Expr
  final case class Unary(s: String, expression: Expr) extends Expr
  final case class Binary(left: Expr, op: String, right: Expr) extends Expr
  final case class Paren(expression: Expr) extends Expr
  final case class Literal(literal: Lit) extends Expr

  sealed trait Statement
  case object Break extends Statement
  case object Continue extends Statement
  case object Empty extends Statement
  final case class IfElse(expression: Expr, ifStmts: List[Statement], elseStmts: List[Statement]) extends Statement
  final case class Return(expressionOpt: Option[Expr]) extends Statement
  final case class While(expression: Expr, statements: List[Statement]) extends Statement
  final case class Expression(expression: Expr) extends Statement

  object Expression {

    def flatten(expr: Expr): Expr = expr match {
      case Index(expression1, expression2) => Index(flatten(expression1), flatten(expression2))
      case Call(expression, arguments)     => Call(flatten(expression), arguments.map(flatten))
      case Unary(s, expression)            => Unary(s, flatten(expression))
      case Binary(left, op, right)         => Binary(flatten(left), op, flatten(right))
      case Paren(expression)               => flatten(expression)
      case Literal(literal)                => Literal(literal)
    }

    def applyExpr(expr: Expr)(f: Expr => Expr): Expr = expr match {
      case Index(expression1, expression2) => Index(f(expression1), f(expression2))
      case Call(expression, arguments)     => Call(f(expression), arguments.map(f))
      case Unary(s, expression)            => Unary(s, f(expression))
      case Binary(left, op, right)         => Binary(f(left), op, f(right))
      case Paren(expression)               => Paren(f(expression))
      case Literal(literal)                => Literal(literal)
    }

    def flattenImproved(expr: Expr): Expr = expr match {
      case Paren(expression) => flattenImproved(expression)
      case xs                => applyExpr(xs)(flattenImproved)
    }

  }

}
