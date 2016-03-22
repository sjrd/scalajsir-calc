package calc

import calc.Typechecker.TypecheckerException
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

  // ----------- Let bindings ------------
  @Test def runSimpleLet(): Unit = {
    assertRun(9, "let x = 4 in x + 5")
  }

  @Test def runNestedLet(): Unit = {
    assertRun(9, "let x = 4 in let y = 5 in x + y")
  }

  @Test def runShadowingLet(): Unit = {
    assertRun(5, "let x = 4 in let x = 5 in x")
  }

  // ----------- Error messages ----------
  @Test(expected = classOf[TypecheckerException]) def runUnboundIdent1(): Unit = {
    assertRun(0.0, "someid")
  }

  @Test(expected = classOf[TypecheckerException]) def runUnboundIdent2(): Unit = {
    assertRun(0.0, "let x = 42 in someid")
  }

  // ----------- If expressions ----------
  @Test def runTrueIf(): Unit = {
    assertRun(42.0, "if (1) 42 else 16")
  }

  @Test def runFalseIf(): Unit = {
    assertRun(16.0, "if (0) 42 else 16")
  }

  // ------- Compound statements ---------
  @Test def runCompoundIf1(): Unit = {
    assertRun(42.0, "let x = 42 in if (1) x else 16")
  }

  @Test def runCompoundIf2(): Unit = {
    assertRun(58.0, "let x = 42 in if (0) x else x + 16")
  }

}
