package calc

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._

/** Tests focused on the Compiler.
 *
 *  You can add more "whitebox" tests here. A whitebox test checks that the
 *  compiler precisely emits the Trees we expect from some input.
 */
class CompilerTest {

  private implicit val DummyPos = ir.Position.NoPosition

  private val MainObjectFullName = Compiler.MainObjectFullName
  private val MainClassFullName = MainObjectFullName + "$"

  // Could be useful in tests, depending on the trees you generate
  private val classType = irtpe.ClassType(encodeClassName(MainClassFullName))

  private def assertCompile(expected: irt.Tree, sourceTree: Tree): Unit = {
    /* IR Trees do not have a meaningful equals() method, so we test equality
     * through hashes.
     */

    def hashOf(body: irt.Tree): irt.TreeHash = {
      // Can only hash entire methods
      val methodDef = irt.MethodDef(static = false, irt.Ident("main__D"),
          Nil, irtpe.DoubleType, body)(
          irt.OptimizerHints.empty, None)
      ir.Hashers.hashMethodDef(methodDef).hash.get
    }

    val expectedHash = hashOf(expected)
    val actual = Compiler.compileExpr(sourceTree)
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
        ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = true))
  }

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
  }

  @Test def compileBinaryOp(): Unit = {
    import irt.BinaryOp._
    assertCompile(
      irt.BinaryOp(Double_+, irt.DoubleLiteral(234), irt.DoubleLiteral(123)),
      BinaryOp("+",Literal(234), Literal(123)))
  }
}
