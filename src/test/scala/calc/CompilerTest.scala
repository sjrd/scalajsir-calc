package calc

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir
import org.scalajs.ir.{Trees => irt, Types => irtpe}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName

/** Tests focused on the Compiler.
 *
 *  You can add more "whitebox" tests here. A whitebox test checks that the
 *  compiler precisely emits the Trees we expect from some input.
 */
class CompilerTest {

  private implicit val DummyPos = ir.Position.NoPosition

  private val MainClassFullName = Compiler.MainClassFullName

  // Could be useful in tests, depending on the trees you generate
  private val classType = irtpe.ClassType(ClassName(MainClassFullName))

  private def assertCompile(expected: irt.Tree, sourceTree: Tree): Unit = {
    /* IR Trees do not have a meaningful equals() method, so we test equality
     * through hashes.
     */

    def hashOf(body: irt.Tree): irt.TreeHash = {
      // Can only hash entire methods
      val methodDef = irt.MethodDef(irt.MemberFlags.empty,
          irt.MethodIdent(MethodName("main", Nil, irtpe.DoubleRef)),
          NoOriginalName, Nil, irtpe.DoubleType, Some(body))(
          irt.OptimizerHints.empty, None)
      ir.Hashers.hashMethodDef(methodDef).hash.get
    }

    val expectedHash = hashOf(expected)
    val actual = Compiler.compileExpr(sourceTree)
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
        ir.Hashers.hashesEqual(actualHash, expectedHash))
  }

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
  }

}
