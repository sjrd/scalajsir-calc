package calc

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir
import org.scalajs.ir.{Trees => irt, Types => irtpe}
import org.scalajs.ir.Names._

import org.scalajs.logging._

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.test.kit.TestKit

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class RunTest {
  private val kit = new TestKit(new NodeJSEnv(), Duration(5L, TimeUnit.SECONDS))

  private def assertRun(expected: Double, code: String): Unit = {
    val tree = Parser.parse(code).get.value
    val classDef = Compiler.compileMainClass(tree)
    val linked = Linker.link(classDef, new ScalaConsoleLogger(Level.Error))
    val input = Seq(Input.Script(linked))

    kit.withRun(input) { run =>
      run
        .expectOut(expected.toString() + "\n")
        .closeRun()
    }
  }

  @Test def runLiteral(): Unit = {
    assertRun(54.3, "54.3")
  }

}
