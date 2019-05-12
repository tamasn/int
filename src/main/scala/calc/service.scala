package calc

import domain._
import cats.data.State
import cats.data.EitherT
import cats.syntax.either._

object service {
  type TokenResult[A] = Either[String, A]

  type Tokenizer[A] = EitherT[State[List[Char], ?], String, A]

  object Interpreter {
    def fail[A]: TokenResult[A] = Either.left("Error parsing input")
    def failM[A](): Tokenizer[A] = EitherT(State(_ => (Nil, Either.left("Error parsing input"))))

    private def resp(rest: List[Char], t: Token): (List[Char], Either[String, Token]) =
      (rest, Either.right[String, Token](t))

    private def getDigits(txt: List[Char], acc: List[Int]): (List[Char], Int) = txt match {
      case Digit(v) :: rest => getDigits(rest, v :: acc)
      case xs =>
        (xs, acc.zipWithIndex.map({ case (digit, pos) => math.pow(10, pos.toDouble) * digit }).sum.toInt)
    }

    private def skipWhitespaces(txt: List[Char]): List[Char] = txt match {
      case ' ' :: rest => skipWhitespaces(rest)
      case xs          => xs
    }

    private def getNextToken0: Tokenizer[Token] =
      EitherT(for {
        _ <- State.modify(skipWhitespaces)
        t <- State[List[Char], Either[String, Token]] {
          case Nil           => resp(Nil, Token.eof)
          case Op(o) :: rest => resp(rest, Token.op(o))
          case Digit(v) :: rest =>
            val (rest0, i) = getDigits(rest, List(v))
            resp(rest0, Token.integer(i))
          case _ => (Nil, fail)
        }
      } yield t)

    def getNextToken[A](expectedType: TokenType.Aux[A]): Tokenizer[A] =
      for {
        token <- getNextToken0
        v <- token match {
          case expectedType(x) => EitherT.right[String](State.inspect[List[Char], A](_ => x))
          case _               => failM
        }
      } yield v

    def expr: Tokenizer[Int] =
      for {
        left <- getNextToken(TokenType.integer)
        op <- getNextToken(TokenType.op)
        right <- getNextToken(TokenType.integer)
      } yield (op.ap(left, right))
  }
}
