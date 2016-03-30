package calc

import org.junit.Test
import TestHelpers._

/** End-to-end tests, with parsing, compiling and running.
 *
 *  You can add more "blackbox" unit testing here. A blackbox test checks that
 *  compiling and running some piece of code produces the expected final
 *  result.
 */
class RunSimpleValue {

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

}
