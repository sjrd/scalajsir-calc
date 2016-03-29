package calc
import scala.util.Try

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._

import org.scalajs.core.tools.logging._

import org.scalajs.jsenv.JSConsole

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class RunTest {

  private def assertRun(expected: Double, code: String): Unit = {
    val tree = Parser.parse(code).get.value
    val classDef = Compiler.compileMainClass(tree)
    val linked = Linker.link(classDef, NullLogger)

    val lines = new java.io.StringWriter
    val console = new JSConsole {
      def log(msg: Any): Unit = lines.append(msg.toString() + "\n")
    }

    Runner.run(linked, NullLogger, console)

    assertEquals(expected.toString(), lines.toString().trim)
  }

  @Test def runLiteral(): Unit = {
    assertRun(54.3, "54.3")
  }

  @Test def runOpBinary(): Unit = {
    assertRun(13.3, "12.3 + (23.12 - 12) / 11.12")
    assertRun(12.2, "6.0 / 3 + 10.2")
    assertRun(-215.68, "0.32 - (23.12 + 12.88) * 6")
  }

  @Test def runLetExpr(): Unit = {
    assertRun(4.41, "let sum = 2.1 in sum * sum")
    assertRun(113.0876, "let s = 2.3 in let t = 9.2323 in (t * 12) + s")
  }

  @Test def runLetException(): Unit = {
    assertTrue(Try {
      assertRun(4.41, "let sum = 2.1 in s * sum")
    }.isFailure)
  }

  @Test def runIfElse(): Unit = {
    assertRun(3.1, "if(1) 3.1 else 2.2")
    assertRun(2.2, "let i = 10 in if(10-i) 123.1 else 2.2")
  }

  @Test def runLambdaNC(): Unit = {
    assertRun(16.80, "let f = fun (x,y) = {x*y} in 4*f(2.1,2)")
    assertRun( 3.20, "let f = fun (x) = { if(x) x else 1.1} in f(3.2)")
    assertRun( 1.10, "let f = fun (x) = { if(x) x else 1.1} in f(0)")
    assertRun(16.81, "let f=fun(x)={x*x} in let x =fun(q) ={q+2} in f(x(2.1))")
  }

  @Test def runNativeFun(): Unit = {
    assertRun(2.2, "sqrt(pow(2.2,2))")
    assertRun(1.1, "pow(sin(2),2) + pow(cos(2),2) + 0.1")
    assertTrue(Try {
      assertRun(4.41, "let sin = fun(x) = {x*x} in sin(2.1)")
    }.isFailure)
  }

  @Test def runLambdaCaptures(): Unit = {
    assertRun(4.2,  "let k = 2 in let f = fun(x) = {k*x} in f(2.1)")
    assertRun(8.2,  "let d = fun(x) = {2 * x} in "+
                    "let addDouble = fun(x,y) = {d(x) + d(y)} in "+
                    "addDouble(2.1,2)"
              )
  }
}
