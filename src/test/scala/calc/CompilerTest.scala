package calc

import org.junit.Test
import org.junit.Assert._
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

  @Test def letBinding() {
    val x = irt.Ident("x", Some("x"))
    val ref_x = irt.VarRef(x)(irtpe.DoubleType)
    val let_x_eq_100_in_x = irt.Block(List(
      irt.VarDef(x, irtpe.DoubleType, false, irt.DoubleLiteral(100.0)), ref_x))
    val let_x_eq_100_in_x_plus_x = irt.Block(List(
      irt.VarDef(x, irtpe.DoubleType, false, irt.DoubleLiteral(100.0)),
      irt.BinaryOp(irt.BinaryOp.Double_+, ref_x, ref_x)))

    assertCompile(let_x_eq_100_in_x, Let(Ident("x"), Literal(100.0), Ident("x")))
    assertCompile(let_x_eq_100_in_x_plus_x, Let(Ident("x"), Literal(100.0), BinaryOp("+", Ident("x"), Ident("x"))))
  }

  @Test def closure() {
    val fun_ret_one = irt.Closure(List(), List(), irt.DoubleLiteral(1.0), List())
    assertCompile(fun_ret_one, Closure(List(), Literal(1.0)))

    val xParam = irt.ParamDef(irt.Ident("x__"), irtpe.AnyType, false, false)
    val xDef = irt.VarDef(irt.Ident("x"), irtpe.DoubleType, false,
      irt.Unbox(irt.VarRef(irt.Ident("x__"))(irtpe.DoubleType), 'D'))
    val xRef = irt.VarRef(irt.Ident("x"))(irtpe.DoubleType)
    val fun_x_ret_x = irt.Closure(List(), List(xParam), irt.Block(List(xDef, xRef)), List())
    val got = Compiler.compileExpr(Closure(List(Ident("x")), Ident("x")))
    assertTrue(got.toString equals fun_x_ret_x.toString)
  }
  
  @Test def ifElse() {
    val one = irt.DoubleLiteral(100.0)
    val cond = irt.BinaryOp(irt.BinaryOp.!==, one, irt.DoubleLiteral(0))
    val if_one_then_one_else_one = irt.If(cond, one, one)(irtpe.DoubleType)
    assertCompile(if_one_then_one_else_one, If(Literal(100.0), Literal(100.0), Literal(100.0)))
  }
}
