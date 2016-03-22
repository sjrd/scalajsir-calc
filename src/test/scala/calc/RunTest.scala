package calc

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import util.Random

import org.scalajs.core.tools.logging._

import org.scalajs.jsenv.JSConsole

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class RunTest {
  val r = Random
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

  @Test def runBinaryOp(): Unit = {
    for (i <- 1 to 5){
      val a = r.nextDouble()
      val b = r.nextDouble()
      assertRun(a + b, s"${a} + ${b}")
      assertRun(a - b, s"${a} - ${b}")
      assertRun(a * b, s"${a} * ${b}")
      assertRun(a / b, s"${a} / ${b}")
    }
  }

  @Test def runLetOp(): Unit = {
    assertRun(123.321 + 456.654, "let x=123.321 in x+456.654")
    assertRun(1.5 + 2.0, "let x=1.5 in let y=2.0 in x+y")
  }

  @Test def runCondOp(): Unit = {
    assertRun(4.2, "if (0) 1.0 else 4.2")
    assertRun(2.4, "if (1) 2.4 else 0.0")
  }

  @Test def runFindCapture(): Unit = {
    def testCapture(p: String, res: Set[String]): Unit ={
      val tree = Parser.parse(p).get.value
      assertEquals(Compiler findCapture tree, res)
    }

    testCapture("let x=1 in x+y", Set("y"))
    testCapture("let x=1 in let y=2 in u+v", Set("u","v"))
    testCapture("let f=fun (x) = {let y=1 in x+y} in f (1)", Set())
    testCapture("let f=fun (g) = {g (1)} in let g=fun (x) = {x+1} in f (g)", Set())
    testCapture("let f=fun (g) = {g (x)} in f (y)", Set("x", "y"))
    testCapture("fun (x) = {x + 1}", Set())
    testCapture("let fac = fun (x) = {if (x) x*fac(x-1) else 1} in fac(5)", Set())
    testCapture("let x=1 in let f=fun(y)={y+x} in f(1)", Set())
    testCapture("let f=fun(y)={y+x} in f(1)", Set("x"))
  }

  @Test def runFunAndCall(): Unit = {
    assertRun(2.5, "let x=1 in let f=fun(y)={y+x} in f(1.5)")
    assertRun(2.5, "let f=fun(g)={g(1.0)} in let g=fun(x)={x+1.5} in f(g)")
  }


}
