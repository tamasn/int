package calc

import cats.Show
import cats.instances.int._

object domain {

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

    val plus = instance[Unit]

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
  }

  def integer(x: Int): Token.Aux[Int] = TokenType.integer.token(x)
  val plus: Token.Aux[Unit] = TokenType.plus.token(())
  val eof: Token.Aux[Unit] = TokenType.eof.token(())

  implicit def tokenShow(): Show[Token] = Show.show {
    case TokenType.integer(x) => s"Token(INTEGER, ${Show[Int].show(x)})"
    case TokenType.plus(_)    => s"Token(PLUS, '+')"
    case TokenType.eof(_)     => s"Token(EOF, ())"
  }

  object Digit {
    def unapply(c: Char): Option[Int] =
      if (c >= 48 && c <= 57) Option(c - 48)
      else None
  }
}
