package calc

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import irt.BinaryOp._

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
    assertCompile(
      irt.BinaryOp(Double_+, irt.DoubleLiteral(234), irt.DoubleLiteral(123)),
      BinaryOp("+",Literal(234), Literal(123)))
  }

  @Test def compileLet(): Unit = {
    val idRef = irt.VarRef(irt.Ident("sum"))(irtpe.DoubleType)

    assertCompile(
      irt.Block(List(
        irt.VarDef(irt.Ident("sum"), irtpe.DoubleType, false, irt.DoubleLiteral(2)),
        irt.BinaryOp(Double_*, idRef, idRef))
      ),
      Let(Ident("sum"), Literal(2), BinaryOp("*", Ident("sum"), Ident("sum")))
    )
  }

  @Test def compileIfElse(): Unit = {
    val cond = irt.BinaryOp(Double_-, irt.DoubleLiteral(2), irt.DoubleLiteral(20))
    val ifCond = irt.BinaryOp(Num_!=, cond , irt.DoubleLiteral(0))

    assertCompile(
      irt.If(ifCond, irt.DoubleLiteral(7), irt.DoubleLiteral(53))(irtpe.DoubleType),
      If(BinaryOp("-",Literal(2),Literal(20)), Literal(7), Literal(53)))
  }   
}
