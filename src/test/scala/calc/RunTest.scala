package calc

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import util.Random

import org.scalajs.core.tools.logging._

import org.scalajs.jsenv.JSConsole
import java.math._

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

  @Test def runBinaryOp(): Unit = {
    assertRun(42.0 + 24.5, "42.0 + 24.5")
    assertRun(42.0 - 24.5, "42.0 - 24.5")
    assertRun(42.5 * 24.5, "42.5 * 24.5")
    assertRun(42.0 / 24.5, "42.0 / 24.5")
  }

  @Test def runLetOp(): Unit = {
    assertRun(123.321 + 456.654, "let x=123.321 in x+456.654")
    assertRun(1.5 + 2.0, "let x=1.5 in let y=2.0 in x+y")
  }

  @Test def runCondOp(): Unit = {
    assertRun(4.2, "if (0) 1.0 else 4.2")
    assertRun(2.4, "if (1) 2.4 else 0.0")
  }

  @Test def runFunAndCall(): Unit = {
    assertRun(2.5, "let x=1 in x+1.5")
    assertRun(2.5, "let x=1 in let f=fun(y)={y+x} in f(1.5)")
    assertRun(2.5, "let x=1 in let y=1.5 in let f=fun(x,y)={y+x} in f(x,y)")
  }

  @Test def runMathFunction(): Unit = {
    assertRun(math.sin(1.0), "let x=1.0 in sin(x)")
    assertRun(math.cos(1.0), "let x=1.0 in cos(x)")
    assertRun(math.log(42.0), "let x=42.0 in log(x)")
    assertRun(math.tan(42.0), "let x=42.0 in tan(x)")
    assertRun(math.pow(42,2.4), "let x=42 in let y=2.4 in pow(x,y)")
    assertRun(math.abs(-42.5), "let x=-42.5 in abs(x)")
    assertRun(math.max(42.5,24.5), "let x=42.5 in let y=24.5 in max(x,y)")
    assertRun(math.min(42.5,24.5), "let x=42.5 in let y=24.5 in min(x,y)")
  }

}
