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

    val foreignEnv = Foreign.staticJavaMethods(
      clsName = "java.lang.Math",
      importedMethods = List(
        ("sin", TFun(1)),
        ("cos", TFun(1)),
        ("sqrt", TFun(1)),
        ("pow", TFun(2))
      )
    )

    // Phase 1: Convert AST to Typed AST
    implicit val typeEnv = Typer.emptyEnv ++ foreignEnv
    val typedAst = Typer.inferType(tree)

    // Phase 2: Compile Typed AST to IR
    expr(typedAst)
  }

  def expr(t: TreeT): irt.Tree = {
    t match {
      case t: IdentT => ident(t)
      case t: LiteralT => literal(t)
      case t: BinaryOpT => binaryOp(t)
      case t: LetT => letBinding(t)
      case t: IfT => ifElse(t)
      case t: ClosureT => closure(t)
      case t: CallT => call(t)
      case t: ForeignCallT => foreignCall(t)
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

  def letBinding(t: LetT) = { implicit val pos = t.pos
    val bindingRhs = expr(t.value)
    val binding = irt.VarDef(irt.Ident(t.name.name), t.value.tpe.irtype, false, bindingRhs)
    val body = expr(t.body)
    irt.Block(List(binding, body))
  }

  def ifElse(t: IfT) = { implicit val pos = t.pos
    val cond = irt.BinaryOp(irt.BinaryOp.!==, expr(t.cond), irt.DoubleLiteral(0))
    val thenp = expr(t.thenp)
    val elsep = expr(t.elsep)
    irt.If(cond, thenp, elsep)(t.tpe.irtype)
  }

  def closure(t: ClosureT) = { implicit val pos = t.pos
    val paramBoxed = t.params map { ident =>
      irt.ParamDef(irt.Ident(ident.name), irtpe.AnyType, false, false)
    }
    val paramUnbox = t.params map { ident =>
      val varRef = irt.VarRef(irt.Ident(ident.name))(irtpe.AnyType)
      val varUnbox = irt.Unbox(varRef, 'D')
      irt.VarDef(irt.Ident(ident.name), irtpe.DoubleType, false, varUnbox)
    }
    val body = expr(t.body)
    val block = irt.Block(paramUnbox ++ List(body))
    irt.Closure(List(), paramBoxed, block, List())
  }

  def call(t: CallT) = { implicit val pos = t.pos
    val args = t.args map expr
    irt.Unbox(irt.JSFunctionApply(expr(t.fun), args), 'D')
  }

  def foreignCall(t: ForeignCallT) = { implicit val pos = t.pos
    val receiver = irt.LoadModule(irtpe.ClassType(encodeClassName(t.clsName + "$")))
    val method = irt.Ident(t.methodName)
    val args = t.args map expr
    irt.Apply(receiver, method, args)(t.tpe.irtype)
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

  private def freeVariables(t: TreeT): Set[String] = {
    case t: IdentT => Set(t.name)
    case t: LiteralT => Set()
    case t: BinaryOpT => freeVariables(t.lhs) ++ freeVariables(t.rhs)
    case t: LetT => (freeVariables(t.value) ++ freeVariables(t.body)) filterNot { _ == t.name }
    case t: IfT => freeVariables(t.cond) ++ freeVariables(t.elsep) ++ freeVariables(t.thenp)
    case t: ClosureT =>
      val params = t.params map { _.name }
      freeVariables(t.body) -- params
    case t: CallT => freeVariables(t.fun) ++ (t.args flatMap freeVariables)
    case t: ForeignCallT => t.args flatMap freeVariables
  }
}


