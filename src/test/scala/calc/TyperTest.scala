package calc

import org.junit.Test
import org.scalajs.core.ir
import ir.{Trees => irt}
import TestHelpers._

/** Tests focused on the Compiler.
  *
  *  You can add more "whitebox" tests here. A whitebox test checks that the
  *  compiler precisely emits the Trees we expect from some input.
  */
class TyperTest {

  @Test def literal(): Unit = {
    assertType(TDouble, Literal(200.0))
  }

  @Test def binaryExpression_positive() {
    val expr_100 = Literal(100.0)
    val expr_50 = Literal(50)
    // Simple Binary Operation
    val expr_100_plus_50 = BinaryOp("+", expr_100, expr_50)
    assertType(TDouble, expr_100_plus_50)
  }
}
