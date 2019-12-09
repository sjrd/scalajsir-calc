package calc

import org.scalajs.ir
import org.scalajs.ir.{Trees => irt, Types => irtpe}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  private val EMF = irt.MemberFlags.empty
  private val EAF = irt.ApplyFlags.empty

  private val mainDoubleMethodName = MethodName("main", Nil, irtpe.DoubleRef)
  private val mainVoidMethodName = MethodName("main", Nil, irtpe.VoidRef)

  final val MainObjectFullName = "main.Main"

  private final val MainClassFullName = MainObjectFullName + "$"

  /** Compile an expression tree into a full `ClassDef`.
   *
   *  You do not need to modify this method.
   */
  def compileMainClass(tree: Tree): irt.ClassDef = {
    import irt.MemberNamespace.Constructor

    implicit val pos = tree.pos

    val className = ClassName(MainClassFullName)
    val classType = irtpe.ClassType(className)

    val ctorDef = irt.MethodDef(EMF.withNamespace(Constructor),
        irt.MethodIdent(NoArgConstructorName), NoOriginalName, Nil, irtpe.NoType,
        Some(irt.Block(List(
            irt.ApplyStatically(EAF.withConstructor(true), irt.This()(classType),
                ObjectClass,
                irt.MethodIdent(NoArgConstructorName),
                Nil)(
                irtpe.NoType),
            irt.StoreModule(className, irt.This()(classType))))))(
        irt.OptimizerHints.empty, None)

    val body = compileExpr(tree)
    val methodDef = irt.MethodDef(EMF,
        irt.MethodIdent(mainDoubleMethodName), NoOriginalName, Nil,
        irtpe.DoubleType, Some(body))(
        irt.OptimizerHints.empty, None)

    val mainMethodDef = irt.MethodDef(EMF,
        irt.MethodIdent(mainVoidMethodName), NoOriginalName, Nil, irtpe.NoType,
        Some {
          val result = irt.Apply(EAF, irt.This()(classType),
              irt.MethodIdent(mainDoubleMethodName), Nil)(irtpe.DoubleType)
          irt.JSMethodApply(
              irt.JSGlobalRef("console"),
              irt.StringLiteral("log"),
              List(result))
        })(
        irt.OptimizerHints.empty, None)

    val allDefs = List(ctorDef, methodDef, mainMethodDef)

    val classDef = irt.ClassDef(
        irt.ClassIdent(className),
        NoOriginalName,
        ir.ClassKind.ModuleClass,
        None,
        Some(irt.ClassIdent(ObjectClass)),
        Nil,
        None,
        None,
        allDefs,
        Nil)(
        irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case Literal(value) =>
        irt.DoubleLiteral(value)

      case _ =>
        throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
