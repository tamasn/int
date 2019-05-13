package calc

import cats.Show
import cats.instances.int._
import cats.FlatMap
import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.annotation.tailrec

object domain {
  sealed trait Expr

  object Expr {
    sealed private trait Tr[A]
    private object Tr {
      case class D[A](x: A) extends Tr[A]
      case class M[A](r: () => Tr[A]) extends Tr[A]

      implicit val trFlatMap: FlatMap[Tr] = new FlatMap[Tr] {
        override def map[A, B](fa: Tr[A])(f: A => B): Tr[B] = fa match {
          case D(x) => D(f(x))
          case M(r) => M(() => map(r())(f))
        }

        override def flatMap[A, B](fa: Tr[A])(f: A => Tr[B]): Tr[B] = fa match {
          case D(x) => f(x)
          case M(r) => M(() => f(interpret(r())))
        }

        @tailrec
        override def tailRecM[A, B](a: A)(f: A => Tr[Either[A, B]]): Tr[B] = f(a) match {
          case D(Right(x)) => D(x)
          case D(Left(x))  => tailRecM(x)(f)
          case M(r) =>
            interpret(r()) match {
              case Left(x)  => tailRecM(x)(f)
              case Right(x) => D(x)
            }
        }
      }

      @tailrec
      def interpret[A](tr: Tr[A]): A = tr match {
        case D(x) => x
        case M(r) => interpret(r())
      }
    }

    final case class Number(x: Int) extends Expr
    final case class Combined(op: Op, x: Expr, y: Expr) extends Expr

    private def evaluate0(e: Expr): Tr[Int] = e match {
      case Number(x) => Tr.D(x)
      case Combined(op, x, y) =>
        Tr.M(() => {
          for {
            x0 <- evaluate0(x)
            y0 <- evaluate0(y)
          } yield op.ap(x0, y0)
        })
    }

    def evaluate(e: Expr): Int = Tr.interpret(evaluate0(e))
  }

  sealed trait Op {
    def ap: (Int, Int) => Int
  }

  object Op {
    def unapply(c: Char): Option[Op] = c match {
      case '+' => Some(Plus)
      case '-' => Some(Minus)
      case '*' => Some(Multiple)
      case '/' => Some(Divide)
      case _   => None
    }
    final case object Plus extends Op {
      override val ap = _ + _
    }

    final case object Minus extends Op {
      override val ap = _ - _
    }

    final case object Multiple extends Op {
      override val ap = _ * _
    }

    final case object Divide extends Op {
      override val ap = _ / _
    }

    implicit val showOp: Show[Op] = Show.show {
      case Plus     => "+"
      case Minus    => "-"
      case Multiple => "*"
      case Divide   => "/"
    }

  }

  sealed trait TokenType {
    type Data

    def token(v: Data): Token.Aux[Data]

    def unapply(t: Token): Option[Data] = if (t.tt == this) Some(t.token.asInstanceOf[Data]) else None
  }

  object TokenType {
    type Aux[A] = TokenType { type Data = A }

    def instance[A]: Aux[A] = new TokenType { self =>
      type Data = A
      override def token(v: Data): Token.Aux[Data] = Token.instance[A](self, v)
    }
    val integer = instance[Int]

    val op = instance[Op]

    val eof = instance[Unit]
  }

  sealed abstract class Token {
    type Data
    val tt: TokenType
    def token: Data
  }

  object Token {
    type Aux[A] = Token { type Data = A }

    def instance[A](tpe: TokenType { type Data = A }, v: A): Aux[A] = new Token {
      type Data = A
      val tt = tpe
      val token = v
    }

    def integer(x: Int): Token.Aux[Int] = TokenType.integer.token(x)
    def op(x: Op): Token.Aux[Op] = TokenType.op.token(x)
    val eof: Token.Aux[Unit] = TokenType.eof.token(())

    implicit def tokenShow[A]: Show[Token.Aux[A]] = Show.show {
      case TokenType.integer(x) => s"Token(INTEGER, ${Show[Int].show(x)})"
      case TokenType.op(c)      => s"Token(OP, '${Show[Op].show(c)}')"
      case TokenType.eof(_)     => s"Token(EOF, ())"
    }
  }

  object Digit {
    def unapply(c: Char): Option[Int] =
      if (c >= 48 && c <= 57) Option(c - 48)
      else None
  }
}
