package calc

import domain._
import cats.data.State
import cats.data.EitherT
import cats.syntax.either._
import scala.annotation.tailrec

object service {
  type TokenResult[A] = Either[String, A]

  type Tokenizer[A] = EitherT[State[List[Char], ?], String, A]

  object Tokenizer {
    def fail[A]: TokenResult[A] = Either.left("Error parsing input")
    def failM[A](): Tokenizer[A] = EitherT(State(_ => (Nil, fail[A])))

    def pure[A](x: A): Tokenizer[A] = EitherT.right(State.pure(x))

  }

  implicit class TokenizerOps(t: Tokenizer[Token]) {
    def attempt[A](tt: TokenType.Aux[A]): Tokenizer[Option[A]] = t.map {
      case tt(x) => Some(x)
      case _     => None
    }

    def must[A](tt: TokenType.Aux[A]): Tokenizer[A] = t.flatMap {
      case tt(x) => Tokenizer.pure(x)
      case _     => Tokenizer.failM()
    }
  }
  object Interpreter {

    private def resp(rest: List[Char], t: Token): (List[Char], Either[String, Token]) =
      (rest, Either.right[String, Token](t))

    @tailrec
    private def getDigits(txt: List[Char], acc: List[Int]): (List[Char], Int) = txt match {
      case Digit(v) :: rest => getDigits(rest, v :: acc)
      case xs =>
        (xs, acc.zipWithIndex.map({ case (digit, pos) => math.pow(10, pos.toDouble) * digit }).sum.toInt)
    }

    @tailrec
    private def skipWhitespaces(txt: List[Char]): List[Char] = txt match {
      case ' ' :: rest => skipWhitespaces(rest)
      case xs          => xs
    }

    private def getNextToken: Tokenizer[Token] =
      EitherT(for {
        _ <- State.modify(skipWhitespaces)
        t <- State[List[Char], Either[String, Token]] {
          case Nil           => resp(Nil, Token.eof)
          case Op(o) :: rest => resp(rest, Token.op(o))
          case Digit(v) :: rest =>
            val (rest0, i) = getDigits(rest, List(v))
            resp(rest0, Token.integer(i))
          case _ => (Nil, Tokenizer.fail)
        }
      } yield t)

    private def expression: Tokenizer[Expr] =
      for {
        left <- getNextToken.must(TokenType.integer).map(Expr.Number.apply)
        opOrEnd = getNextToken
        opO <- opOrEnd.attempt(TokenType.op)
        expr <- opO match {
          case Some(op) =>
            expression.map(right => Expr.Combined(op, left, right))
          case None => opOrEnd.must(TokenType.eof).map(_ => left)
        }
      } yield expr

    def run(s: String): Either[String, Int] =
      Interpreter.expression.value.runA(s.toList).value.map(Expr.evaluate)
  }
}
