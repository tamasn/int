package calc

import org.scalatest._
import service._

class InterpreterSpec extends FlatSpec with Matchers {
  "Interpreter" should "solve that 4+5=9" in {
    Interpreter.run("4+5") shouldBe Right(9)
  }

  val tests: List[(String, Int)] =
    List(
      "3+4" -> 7,
      "3+5" -> 8,
      "3+9" -> 12,
      "4-2" -> 2,
      "12+24" -> 36,
      "572 - 210 " -> 362,
      "2 * 3" -> 6,
      "3 / 2" -> 1,
      "2 + 3 + 5" -> 10,
      "2 + 3 * 5" -> 17,
      "2 * 3 + 5" -> 16
    )

  tests.map {
    case (pat, res) =>
      it should s"calculate that $pat = $res" in {
        Interpreter.run(pat) shouldBe Right(res)
      }

  }

  it should "fail on 4a2" in {
    Interpreter.run("4a2") shouldBe Tokenizer.fail[Int]
  }
}
