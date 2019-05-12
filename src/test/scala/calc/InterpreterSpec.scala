package calc

import org.scalatest._
import service._

class InterpreterSpec extends FlatSpec with Matchers {
  "Interpreter" should "solve that 4+5=9" in {
    Interpreter.expr.value.runA("4+5".toList).value shouldBe Right(9)
  }

  val tests: List[(String, Int)] = List("3+4" -> 7, "3+5" -> 8, "3+9" -> 12, "4-2" -> 2)
  
  tests.map { case (pat, res) =>
    it should s"calculate that $pat=$res" in {
      Interpreter.expr.value.runA(pat.toList).value shouldBe Right(res)
    }
  
  }

  it should "fail on 4*2" in {
    Interpreter.expr.value.runA("4*2".toList).value shouldBe Interpreter.fail[Int]
  }
}
