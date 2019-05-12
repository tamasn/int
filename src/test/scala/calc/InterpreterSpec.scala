package calc

import org.scalatest._
import service._

class InterpreterSpec extends FlatSpec with Matchers {
  "Interpreter" should "solve that 4+5=9" in {
    Interpreter.expr.value.runA("4+5".toList).value shouldBe (Right(9))
  }

  it should "fail on 4-2" in {
    Interpreter.expr.value.runA("4-2".toList).value shouldBe Interpreter.fail[Int]
  }
}
