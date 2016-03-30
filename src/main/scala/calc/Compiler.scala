package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  final val MainObjectFullName = "main.Main"
  private final val MainClassFullName = MainObjectFullName + "$"

  /** Compile an expression tree into a full `ClassDef`.
   *
   *  You do not need to modify this method.
   */
  def compileMainClass(tree: Tree): irt.ClassDef = {
    implicit val pos = tree.pos

    val className = encodeClassName(MainClassFullName)
    val classType = irtpe.ClassType(className)

    val ctorDef = irt.MethodDef(static = false,
        irt.Ident("init___", Some("<init>")), Nil, irtpe.NoType,
        irt.Block(List(
            irt.ApplyStatically(irt.This()(classType),
                irtpe.ClassType(ObjectClass),
                irt.Ident("init___", Some("<init>")),
                Nil)(
                irtpe.NoType),
            irt.StoreModule(classType, irt.This()(classType)))))(
        irt.OptimizerHints.empty, None)

    val body = compileExpr(tree)
    val methodDef = irt.MethodDef(static = false,
        irt.Ident("main__D", Some("main")), Nil, irtpe.DoubleType, body)(
        irt.OptimizerHints.empty, None)

    val exportedMethodDef = irt.MethodDef(static = false,
        irt.StringLiteral("main"), Nil, irtpe.AnyType,
        irt.Apply(irt.This()(classType), irt.Ident("main__D", Some("main")),
            Nil)(irtpe.DoubleType))(
        irt.OptimizerHints.empty, None)

    val exportedModuleDef = irt.ModuleExportDef(MainObjectFullName)

    val allDefs = List(ctorDef, methodDef, exportedMethodDef, exportedModuleDef)

    val classDef = irt.ClassDef(
        irt.Ident(className),
        ir.ClassKind.ModuleClass,
        Some(irt.Ident(ObjectClass)),
        Nil,
        None,
        allDefs)(
        irt.OptimizerHints.empty)

    ir.Hashers.hashClassDef(classDef)
  }

  /** Compile an expression tree into an IR `Tree`, which is an expression
    *  that evaluates to the result of the tree.
    *
    *  This is the main method you have to implement.
    */
  def compileExpr(tree: Tree): irt.Tree = {
    implicit val pos = tree.pos
    implicit val env = Typer.emptyEnv
    expr(Typer.inferType(tree))
  }

  def expr(t: TreeT): irt.Tree = {
    t match {
      case t: IdentT => ident(t)
      case t: LiteralT => literal(t)
      case t: BinaryOpT => binaryOp(t)
      case t: LetT => letBinding(t)
    }
  }

  def ident(t: IdentT): irt.Tree = { implicit val pos = t.pos
    irt.VarRef(irt.Ident(t.name))(t.tpe.irtype)
  }

  def literal(t: LiteralT) = { implicit val pos = t.pos
    irt.DoubleLiteral(t.value)
  }

  def binaryOp(t: BinaryOpT) = { implicit val pos = t.pos
    val irOp = operatorToIR(t.op) getOrElse (throw UnknownOperator(pos, t.op))
    irt.BinaryOp(irOp, expr(t.lhs), expr(t.rhs))
  }

  def letBinding(t: LetT) = { implicit  val pos = t.pos
    val bindingRhs = expr(t.value)
    val binding = irt.VarDef(irt.Ident(t.name.name), t.value.tpe.irtype, false, bindingRhs)
    val body = expr(t.body)
    irt.Block(List(binding, body))
  }

  private def operatorToIR(op: String) = {
    op match {
      case "+" => Some(irt.BinaryOp.Double_+)
      case "-" => Some(irt.BinaryOp.Double_-)
      case "*" => Some(irt.BinaryOp.Double_*)
      case "/" => Some(irt.BinaryOp.Double_/)
      case _ => None
    }
  }
}

