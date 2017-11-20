package recursionschemes


private object ParameterizedSyntaxTree {

  sealed trait Lit
  final case class StringLiteral(s: String) extends Lit
  final case class IntLiteral(i: Int) extends Lit
  final case class Identity(s: String) extends Lit

  sealed trait Expr[A]
  final case class Index[A](expression1: A, expression2: A) extends Expr[A]
  final case class Call[A](expression: A, arguments: List[A]) extends Expr[A]
  final case class Unary[A](s: String, expression: A) extends Expr[A]
  final case class Binary[A](left: A, op: String, right: A) extends Expr[A]
  final case class Paren[A](expression: A) extends Expr[A]
  final case class Literal[A](literal: Lit) extends Expr[A]

  // def applyExpr[A, B](expr: Expr[A])(f: A => B): Expr[B] ----> Functor!

  // Could be used from Cats or Scalaz
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  // Could be derived with Kittens - https://github.com/milessabin/kittens
  implicit val exprFunctor: Functor[Expr] = new Functor[Expr] {

    override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] =
      fa match {
        case Index(expression1, expression2) => Index(f(expression1), f(expression2))
        case Call(expression, arguments)     => Call(f(expression), arguments.map(f))
        case Unary(s, expression)            => Unary(s, f(expression))
        case Binary(left, op, right)         => Binary(f(left), op, f(right))
        case Paren(expression)               => Paren(f(expression))
        case Literal(literal)                => Literal(literal)
      }

  }

  final case class Term[F[_]](inner: F[Term[F]])

  def bottomUp[F[_]](term: Term[F])(f: Term[F] => Term[F])(implicit functor: Functor[F]): Term[F] =
    f(Term(functor.map(term.inner)(bottomUp(_)(f))))

  def topDown[F[_]](term: Term[F])(f: Term[F] => Term[F])(implicit functor: Functor[F]): Term[F] =
    Term(functor.map(f(term).inner)(topDown(_)(f)))

  def flattenTerm(term: Term[Expr]): Term[Expr] =
    term.inner match {
      case Paren(expression) => expression
      case other             => Term(other)
    }

  def flatten(term: Term[Expr]): Term[Expr] = bottomUp(term)(flattenTerm)


//  | Index   { target :: a, idx :: a }
//  | Unary   { op :: String, target :: a }
//  | Binary  { lhs :: a, op :: String, rhs :: a }
//  | Call    { func :: a, args :: [a] }
//  | Paren   { target :: a }
//  deriving (Show, Eq, Functor)

  sealed trait Expr2[A]
  final case class Literal2[A](intVal: Int) extends Expr2[A]
  final case class Ident2[A](name: String) extends Expr2[A]
  final case class Index2[A](target: A, idx: A) extends Expr2[A]
  final case class Unary2[A](op: String, target: A) extends Expr2[A]
  final case class Binary2[A](lhs: A, op: String, rhs: A) extends Expr2[A]
  final case class Call2[A](func: A, args: List[A]) extends Expr2[A]
  final case class Paren2[A](target: A) extends Expr2[A]

  //mystery :: Functor f => (f a -> a) -> Term f -> a
  def mystery[F[_], A](term: Term[F])(f: F[A] => A)(implicit functor: Functor[F]): A =
    f(functor.map(term.inner)(mystery(_)(f)))

}



