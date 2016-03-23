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
    val actual = Compiler.compileExpr(sourceTree)()
    val actualHash = hashOf(actual)

    assertTrue(s"Expected $expected but got $actual",
        ir.Hashers.hashesEqual(actualHash, expectedHash, considerPos = true))
  }

  @Test def compileLiteral(): Unit = {
    assertCompile(irt.DoubleLiteral(234), Literal(234))
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

}
