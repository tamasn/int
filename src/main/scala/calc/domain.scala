package calc

import cats.Show
import cats.instances.int._

object domain {

  sealed trait TokenType {
    type Data

    def unapply(t: Token): Option[Data] = if (t.tt != this) Some(t.token.asInstanceOf[Data]) else None
  }

  object TokenType {
    case object Integer extends TokenType {
      type Data = Int
    }
    case object Plus extends TokenType {
      type Data = Unit
    }
    case object Eof extends TokenType {
      type Data = Unit
    }
  }

  sealed abstract class Token {
    val tt: TokenType
    def token: tt.Data
  }

  object Token {
    def instance[A](tpe: TokenType { type Data = A }, v: A): Token = new Token {
      val tt = tpe
      val token = v
    }

    def integer(x: Int) = instance(TokenType.Integer, x)
    val plus = instance(TokenType.Plus, ())
    val eof = instance(TokenType.Eof, ())
  }

  implicit def tokenShow: Show[Token] = Show.show {
    case TokenType.Integer(x) => s"Token(INTEGER, ${Show[Int].show(x)})"
    case TokenType.Plus(_)    => s"Token(PLUS, '+')"
    case TokenType.Eof(_)     => s"Token(EOF, ())"
  }

  object Digit {
    def unapply(c: Char): Option[Int] =
      if (c >= 48 && c <= 57) Option(c - 48)
      else None
  }
}
