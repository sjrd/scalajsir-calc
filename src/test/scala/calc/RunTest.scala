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

  private val equalsTolerance: Double = 0.00001

  private def assertRun(expected: Double, code: String): Unit = {
    val tree = Parser.parse(code).get.value
    val classDef = Compiler.compileMainClass(tree)
    val linked = Linker.link(classDef, NullLogger)

    val lines = new java.io.StringWriter
    val console = new JSConsole {
      def log(msg: Any): Unit = lines.append(msg.toString() + "\n")
    }

    Runner.run(linked, NullLogger, console)

    assertEquals(expected, lines.toString().trim.toDouble, equalsTolerance)
  }

  @Test def runLiteral(): Unit = {
    assertRun(54.3, "54.3")
  }

  // ---------- Arithmetic ---------------
  @Test def runSimpleSum(): Unit = {
    assertRun(10.1, "4.5 + 5.6")
  }

  @Test def runSimpleSub(): Unit = {
    assertRun(1.5, "5.5 - 4")
  }

  @Test def runSimpleMul(): Unit = {
    assertRun(25.2, "4.5 * 5.6")
  }

  @Test def runSimpleDiv(): Unit = {
    assertRun(2, "14.0 / 7.0" )
  }

  @Test def runPrecedence1(): Unit = {
    assertRun(6, "2 + 2 * 2")
  }

  @Test def runPrecedence2(): Unit = {
    assertRun(8, "(2 + 2) * 2")
  }

}
