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

    private def getNextToken0: Tokenizer[Token] =
      EitherT(State {
        case Nil                   => resp(Nil, TokenType.eof.token(()))
        case p :: rest if p == '+' => resp(rest, TokenType.plus.token(()))
        case Digit(v) :: rest      => resp(rest, TokenType.integer.token(v))
        case _                     => (Nil, fail)
      })

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
        _ <- getNextToken(TokenType.plus)
        right <- getNextToken(TokenType.integer)
      } yield (left + right)
  }
}
