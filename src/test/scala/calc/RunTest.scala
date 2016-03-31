package calc

import org.junit.Test
import TestHelpers._

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class RunTest {

  @Test def literal() { implicit val comparison = ExactString
    assertRun(54.3, "54.3")
  }

  @Test def binaryExpression() { implicit val comparison = ApproxDouble
    assertRun(150.0, "100.0 + 50.0")
    assertRun(50.0, "100.0 - 50.0")
    assertRun(2.0, "100.0 / 50.0")
    assertRun(5000.0, "100.0 * 50.0")
    assertRun(4979.633, "(100.0 * 50.0) - 20.5 + 0.133")
  }

  @Test def letBinding() { implicit val comparison = ApproxDouble
    assertRun(100.0, "let x = 100.0 in x")
    assertRun(200.0, "let x = 100.0 in x + x")
    assertRun(500.0, "let x = 100.0 in let x = x + x in x + 300")
    assertRun(300.0, "let x = 100.0 in let y = x + x in x + y")
  }

  @Test(expected = classOf[UnboundVariable]) def letBinding_negative() { implicit val comparison = ApproxDouble
    assertRun(100.0, "let x = 100.0 in let y = x + x in x + z")
  }

  @Test def ifElse() { implicit val comparison = ApproxDouble
    assertRun(100.0, "if(1.0) 100.0 else 200.0")
    assertRun(200.0, "if(0) 100.0 else 200.0")
  }

  @Test def ifElsePlusLet() { implicit val comparison = ApproxDouble
    assertRun(200.0, "let x = 200 in if (x - 200) 100.0 else 200.0")
    assertRun(4, "let x = if (3) 2 else 1 in x + x" )
  }

  @Test def closure() { implicit val comparison = ApproxDouble
    assertRun(200.0, "(fun () = { 200 })()")
    assertRun(200.0, "(fun (x, y) = { 200 })(2, 3)")
    assertRun(200.0, "(fun (x, y) = { x + y })(100, 100)")
  }

  @Test def closurePlusLet() { implicit val comparison = ApproxDouble
    assertRun(200.0, "let f = fun () = { 200 } in f()")
    assertRun(200.0, "let f = fun (x) = { 200 } in f(400)")
    assertRun(200.0, "let f = fun (x) = { (x - 200) } in f(400)")
    assertRun(400.0, "let f = fun (x) = { (x - 200) } in let g = fun (f) = { (f - 200) } in g(400) + f(400)")
  }

  @Test def closureCapturing() { implicit val comparison = ApproxDouble
    assertRun(5.0,
      """
        | let f = fun (x, y) = { x + y } in
        | let z = fun () = { f(2, 3) } in z()
      """.stripMargin)
    assertRun(12.0,
      """
        | let f = fun (x, y) = { x + y } in
        | let z = fun () = {
        |   let g = fun(x) = {
        |      x + f(4, 5)
        |   } in
        |   g(3)
        | } in z()
      """.stripMargin)
  }

  @Test def foreignCall() { implicit val comparison = ApproxDouble
    assertRun(1.0, "cos(0)")
    assertRun(0.0, "sin(0)")
    assertRun(2.0, "sqrt(4.0)")
    assertRun(16.0, "pow(2.0, 4.0)")
    assertRun(1.0, "let sin = 0 in cos(sin)")
    assertRun(2.0, "(fun (x) = { sqrt(x) })(4.0)")
  }
}
