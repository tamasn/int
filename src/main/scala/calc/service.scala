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
        case Nil                   => resp(Nil, Token.eof)
        case p :: rest if p == '+' => resp(rest, Token.plus)
        case Digit(v) :: rest      => resp(rest, Token.integer(v))
        case _                     => (Nil, fail)
      })

    def getNextToken(expectedType: TokenType): Tokenizer[expectedType.Data] =
      for {
        token <- getNextToken0
        v <- if (token.tt == expectedType)
          EitherT.right[String](State.pure[List[Char], expectedType.Data](token.token.asInstanceOf[expectedType.Data]))
        else failM()
      } yield v

    def expr: Tokenizer[Int] =
      for {
        left <- getNextToken(TokenType.Integer)
        _ <- getNextToken(TokenType.Plus)
        right <- getNextToken(TokenType.Integer)
      } yield (left + right)
  }
}
