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

  /** Uncomment this later when case analysis of closure is implemented
  @Test(expected = classOf[TypeError])
  def binaryExpression_negative() {
    implicit val env = Typer.emptyEnv
    val expr_100 = Literal(100.0)
    val lambda = Closure(List(Ident("x")), Literal(100.0))
    val ill_typed = BinaryOp("+", lambda, expr_100)
    Typer.inferType(ill_typed)
  }
  **/

  @Test def letBinding() {
    val let_x_eq_100_in_x_plus_x = Let(Ident("x"), Literal(100.0), BinaryOp("+", Ident("x"), Ident("x")))
    val let_x_eq_100_in_100 = Let(Ident("x"), Literal(100.0), Literal(100.0))
    assertType(TDouble, let_x_eq_100_in_x_plus_x)
    assertType(TDouble, let_x_eq_100_in_100)
  }

  @Test(expected = classOf[UnboundVariable]) def letBinding_negative() {
    implicit val env = Typer.emptyEnv
    val let_x_eq_100_in_x_plus_y = Let(Ident("x"), Literal(100.0), BinaryOp("+", Ident("x"), Ident("y")))
    Typer.inferType(let_x_eq_100_in_x_plus_y)
  }

  @Test def ifElse() {
    val if_two_then_three_else_four = If(Literal(2.0), Literal(3.0), Literal(4.0))
    assertType(TDouble, if_two_then_three_else_four)

    val expr_100 = Literal(100.0)
    val expr_100_plus_if = BinaryOp("+", expr_100, if_two_then_three_else_four)
    assertType(TDouble, expr_100_plus_if)
  }

  @Test def closure() {
    val fun_ret_1 = Closure(List(), Literal(20.0))
    assertType(TFun(0), fun_ret_1)
    val fun_x_ret_x = Closure(List(Ident("x")), Ident("x"))
    assertType(TFun(1), fun_x_ret_x)
    val fun_x_y_ret_x = Closure(List(Ident("x"), Ident("y")), Ident("x"))
    assertType(TFun(2), fun_x_y_ret_x)
  }
}
