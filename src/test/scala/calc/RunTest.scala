package calc

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
}
