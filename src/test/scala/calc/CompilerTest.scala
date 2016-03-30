package calc

import org.junit.Test
import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import TestHelpers._

/** Tests focused on the Compiler.
 *
 *  You can add more "whitebox" tests here. A whitebox test checks that the
 *  compiler precisely emits the Trees we expect from some input.
 */
class CompilerTest {

  @Test def literal(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
    assertCompile(irt.DoubleLiteral(234.0), Literal(234.0))
    assertCompile(irt.DoubleLiteral(Double.NaN), Literal(Double.NaN))
    assertCompile(irt.DoubleLiteral(Double.NegativeInfinity), Literal(Double.NegativeInfinity))
    assertCompile(irt.DoubleLiteral(Double.PositiveInfinity), Literal(Double.PositiveInfinity))
  }

  @Test def binaryExpression_positive() {
    val expr_100 = irt.DoubleLiteral(100.0)
    val expr_50 = irt.DoubleLiteral(50)

    // Simple Binary Operation
    val expr_100_plus_50 = irt.BinaryOp(irt.BinaryOp.Double_+, expr_100, expr_50)
    val expr_100_times_50 = irt.BinaryOp(irt.BinaryOp.Double_*, expr_100, expr_50)
    val expr_100_div_50 = irt.BinaryOp(irt.BinaryOp.Double_/, expr_100, expr_50)
    val expr_100_minus_50 = irt.BinaryOp(irt.BinaryOp.Double_-, expr_100, expr_50)
    assertCompile(expr_100_plus_50, BinaryOp("+", Literal(100.0), Literal(50.0)))
    assertCompile(expr_100_times_50, BinaryOp("*", Literal(100.0), Literal(50.0)))
    assertCompile(expr_100_div_50, BinaryOp("/", Literal(100.0), Literal(50.0)))
    assertCompile(expr_100_minus_50, BinaryOp("-", Literal(100.0), Literal(50.0)))

    // Nested Binary Operation
    val nestedRight = irt.BinaryOp(irt.BinaryOp.Double_*, expr_50, expr_100_plus_50)
    assertCompile(nestedRight, BinaryOp("*", Literal(50.0), BinaryOp("+", Literal(100.0), Literal(50.0))))

    val nestedLeft = irt.BinaryOp(irt.BinaryOp.Double_*, expr_100_plus_50, expr_50)
    assertCompile(nestedLeft, BinaryOp("*", BinaryOp("+", Literal(100.0), Literal(50.0)), Literal(50.0)))
  }

  @Test(expected = classOf[UnknownOperator])
  def binaryExpression_negative(): Unit = {
    // Invalid Operator
    Compiler.compileExpr(BinaryOp("&&", Literal(100.0), Literal(50.0)))
  }
}
