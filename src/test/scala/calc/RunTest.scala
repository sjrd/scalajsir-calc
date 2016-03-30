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

  @Test def runNestedIf1(): Unit = {
    assertRun(42.0, "if (if (5) 16 else 0) 42.0 else 34.5")
  }

  @Test def runNestedIf2(): Unit = {
    assertRun(34.5, "if (if (0) 16 else 0) 42.0 else 34.5")
  }

  // ------- Compound statements ---------
  @Test def runCompoundIf1(): Unit = {
    assertRun(42.0, "let x = 42 in if (1) x else 16")
  }

  @Test def runCompoundIf2(): Unit = {
    assertRun(58.0, "let x = 42 in if (0) x else x + 16")
  }

  @Test def runCompoundIf3(): Unit = {
    assertRun(36.0, "if (let x = 15 in x) 36.0 else 15.0")
  }

  @Test def runCompoundIf4(): Unit = {
    assertRun(15.0, "if (let x = 14 in x - 14) 36.0 else 15.0")
  }

  // -------- Lambdas --------------------
  @Test def runSimpleLambda0(): Unit = {
    assertRun(42.0, "let f = fun() = { 42 } in f()")
  }

  @Test def runSimpleLambda1(): Unit = {
    assertRun(42.0, "let f = fun(x) = { x + 1 } in f(41)")
  }

  @Test def runSimpleLambda2(): Unit = {
    assertRun(42.0, "let f = fun(x, y) = { x + y } in f(20, 22)")
  }

  @Test def runLambdaCapture1(): Unit = {
    assertRun(42.0, "let y = 10 in let f = fun(x) = { x + y } in f(32)")
  }

  @Test def runLambdaCapture2(): Unit = {
    assertRun(42.0, "let x = 5 in let y = 10 in let f = fun(z) = { x + y + z } in f(27)")
  }

  @Test def runLambdaCapture3(): Unit = {
    assertRun(42.0, "let f = fun(x) = { let y = 10 in x + y } in f(32)")
  }

  @Test def runLambdaShadowing(): Unit = {
    assertRun(42.0, "let x = 10 in let f = fun(x) = { x } in f(42)")
  }

  @Test def runLambdaCompound1(): Unit = {
    assertRun(3.0, "let g = fun(x) = {x + 1} in let f = fun(x) = {g(x) + 2} in f(0)")
  }

  @Test def runLambdaCompound2(): Unit = {
    assertRun(4.0, "if (let f = fun(x) = {x} in f (10)) 4.0 else 2.0")
  }

  @Test def runLambdaAnonymous(): Unit = {
    assertRun(42.0, "(fun(x) = {x})(42.0)")
  }

  @Test def runLambdaAnonymousLet(): Unit = {
    assertRun(42.0, "(let f = fun(x) = {x} in f)(42.0)")
  }

  @Test def runLambdaAnonymousIf(): Unit = {
    assertRun(42.0, "(if (1) (fun(x) = {x}) else (fun(x) = {0}))(42.0)")
  }

  // -------------- MATH FUNCTIONS --------------------------
  @Test def runSimpleMath1(): Unit = {
    assertRun(Math.sin(1.0), "sin(1.0)")
  }

  @Test def runSimpleMath2(): Unit = {
    assertRun(1.0, "sin(42.0) * sin(42.0) + cos(42.0) * cos(42.0)")
  }

  @Test def runSimpleMath3(): Unit = {
    assertRun(42.0, "exp(log(42.0))")
  }

  @Test def runMathAssignment(): Unit = {
    assertRun(Math.sin(1.0), "let f = sin in f(1.0)")
  }

  @Test def runMathIf(): Unit = {
    assertRun(Math.sin(1.0), "(if (1) sin else cos)(1.0)")
  }

  @Test def runMathShadowing(): Unit = {
    assertRun(42.0, "let sin = fun(x) = { x } in sin(42.0)")
  }
}
