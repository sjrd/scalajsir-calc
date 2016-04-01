package calc

import org.scalajs.core.ir
import ir.{Position, Trees => irt, Types => irtpe}
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
  def compileExpr(tree: Tree, etaExpansion: Boolean = true): irt.Tree = {
    implicit val pos = tree.pos

    val (foreignTypeEnv, foreignEnv) = Foreign.staticJavaMethods(
      clsName = "java.lang.Math",
      importedMethods = List(
        ("sin", TFun(1)),
        ("cos", TFun(1)),
        ("sqrt", TFun(1)),
        ("pow", TFun(2))
      )
    )

    // Phase 1: Convert AST to Typed AST
    implicit val typeEnv = Typer.emptyEnv ++ foreignTypeEnv
    val (typedAst, _) = Typer.inferType(tree)

    // Phase 2: Compile Typed AST to IR
    implicit val foreignImportEnv = foreignEnv
    expr(typedAst)
  }

  def expr(t: TreeT)(implicit env: Foreign.Env): irt.Tree = {
    t match {
      case t: IdentT => ident(t)
      case t: LiteralT => literal(t)
      case t: BinaryOpT => binaryOp(t)
      case t: LetT => letBinding(t)
      case t: IfT => ifElse(t)
      case t: ClosureT => closure(t)
      case t: CallT => call(t)
      case t: SelectT => select(t)
    }
  }

  def ident(t: IdentT)(implicit env: Foreign.Env): irt.Tree = { implicit val pos = t.pos
    env.get(t.name) match {
      case Some(func) => func match {
        case func:StaticForeignFunction => etaExpandStaticFunction(func)
      }
      case None => irt.VarRef(irt.Ident(t.name))(t.tpe.irtype)
    }
  }

  def literal(t: LiteralT)(implicit env: Foreign.Env) = { implicit val pos = t.pos
    irt.DoubleLiteral(t.value)
  }

  def binaryOp(t: BinaryOpT)(implicit env: Foreign.Env) = { implicit val pos = t.pos
    val irOp = operatorToIR(t.op) getOrElse (throw UnknownOperator(pos, t.op))
    irt.BinaryOp(irOp, expr(t.lhs), expr(t.rhs))
  }

  def letBinding(t: LetT)(implicit env: Foreign.Env) = { implicit val pos = t.pos
    val body = expr(t.body)(env - t.name.name)
    t.value match {
      case tree:ClosureT =>
        // Create empty object with name f__ref
        val emptyObject = irt.JSObjectConstr(List())
        val bindingName = t.name.name
        val objectName = irt.Ident(bindingName + "__ref")
        val objectDef = irt.VarDef(objectName, irtpe.AnyType, false, emptyObject)

        // Do f__ref.func = f
        val objectRef = irt.VarRef(objectName)(irtpe.AnyType)
        val dotSelect = irt.JSDotSelect(objectRef, irt.Ident("func"))
        val recursiveCapture = Some(objectName.name, objectRef)

        // Substitute f with f__ref.func in closure body
        val newBody = substitute(tree, t.name.name, new SelectT(objectName.name, "func") { val tpe = tree.tpe })
        val assign = irt.Assign(dotSelect, closure(newBody.asInstanceOf[ClosureT], recursiveCapture))

        // Do the binding, i.e f = f__ref.func
        val attrAccess = irt.JSDotSelect(objectRef, irt.Ident("func"))
        val binding = irt.VarDef(irt.Ident(bindingName), t.value.tpe.irtype, false, attrAccess)
        irt.Block(List(objectDef, assign, binding, body))
      case _ => {
        val bindingRhs = expr(t.value)
        val binding = irt.VarDef(irt.Ident(t.name.name), t.value.tpe.irtype, false, bindingRhs)
        irt.Block(List(binding, body))
      }
    }
  }

  def ifElse(t: IfT)(implicit env: Foreign.Env) = { implicit val pos = t.pos
    val cond = irt.BinaryOp(irt.BinaryOp.!==, expr(t.cond), irt.DoubleLiteral(0))
    val thenp = expr(t.thenp)
    val elsep = expr(t.elsep)
    irt.If(cond, thenp, elsep)(t.tpe.irtype)
  }

  def closure(t: ClosureT, recursiveCapture: Option[(String, irt.VarRef)] = None)
             (implicit env: Foreign.Env) = { implicit val pos = t.pos
    val paramBoxed = boxedParams(t.params)

    val freeVars = freeVariables(t) filter { v => !env.isDefinedAt(v._1) }
    val captureParamsRef = freeVars.map({ v => irt.VarRef(irt.Ident(v._1))(v._2.irtype) }).toList
    val recursiveCaptureRef = (recursiveCapture map { kv => kv._2 }).toList

    val captureParamsDef = freeVars.map({ v => irt.ParamDef(irt.Ident(v._1), v._2.irtype, false, false) }).toList
    val recursiveCaptureDef = (recursiveCapture map { kv => irt.ParamDef(irt.Ident(kv._1), irtpe.AnyType, false, false) }).toList

    val paramUnbox = unboxedParams(t.params)
    val body = expr(t.body)
    val block = irt.Block(paramUnbox ++ List(body))

    irt.Closure(captureParamsDef ++ recursiveCaptureDef, paramBoxed, block,
      captureParamsRef ++ recursiveCaptureRef)
  }

  def call(t: CallT)(implicit env: Foreign.Env) = { implicit val pos = t.pos
    val args = t.args map expr
    irt.Unbox(irt.JSFunctionApply(expr(t.fun), args), 'D')
  }

  def foreignCall(clsName: String, methodName: String, args: List[TreeT])
                 (implicit pos: Position, env: Foreign.Env) = {
    val receiver = irt.LoadModule(irtpe.ClassType(encodeClassName(clsName + "$")))
    val method = irt.Ident(methodName)
    irt.Apply(receiver, method, args map expr)(irtpe.DoubleType)
  }

  def etaExpandStaticFunction(func: StaticForeignFunction)(implicit pos: Position, env: Foreign.Env) = {
    val params = (1 to func.tpe.arity).map({ p =>
      new IdentT("a" + p) { val tpe = TDouble }
    }).toList
    val body = foreignCall(func.clsName, func.methodName, params)
    val paramsBoxed = boxedParams(params)
    val paramsUnboxed = unboxedParams(params)
    val block = irt.Block(paramsUnboxed ++ List(body))
    irt.Closure(Nil, paramsBoxed, block, Nil)
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

  private def select(t: SelectT) = { implicit val pos = t.pos
    irt.JSDotSelect(irt.VarRef(irt.Ident(t.receiver))(irtpe.AnyType), irt.Ident(t.ident))
  }

  private def mangle(name: String) = name + "__"

  private def freeVariables(t: TreeT): Map[String, Type] = {
    t match {
      case t: IdentT => Map(t.name -> t.tpe)
      case t: LiteralT => Map.empty
      case t: SelectT => Map.empty
      case t: BinaryOpT => freeVariables(t.lhs) ++ freeVariables(t.rhs)
      case t: LetT => (freeVariables(t.value) ++ freeVariables(t.body)) filterKeys { _ != t.name.name }
      case t: IfT => freeVariables(t.cond) ++ freeVariables(t.elsep) ++ freeVariables(t.thenp)
      case t: ClosureT =>
        val paramsName = t.params map { _.name }
        freeVariables(t.body) filterKeys { !paramsName.contains(_) }
      case t: CallT => freeVariables(t.fun) ++ (t.args flatMap freeVariables)
    }
  }

  private def boxedParams(params: List[IdentT])(implicit pos: Position) = {
    params map { ident =>
      val mangled = irt.Ident(mangle(ident.name))
      irt.ParamDef(mangled, irtpe.AnyType, false, false)
    }
  }

  private def unboxedParams(params: List[IdentT])(implicit pos: Position) = {
    params map { ident =>
      val mangled = irt.Ident(mangle(ident.name))
      val varRef = irt.VarRef(mangled)(irtpe.AnyType)
      val varUnbox = irt.Unbox(varRef, 'D')
      irt.VarDef(irt.Ident(ident.name), irtpe.DoubleType, false, varUnbox)
    }
  }

  private def substitute(tree: TreeT, name: String, withTree: SelectT): TreeT = { implicit val pos = tree.pos
    tree match {
      case t: LiteralT => t
      case t: SelectT => t
      case t: IdentT => if (t.name == name) { withTree } else t
      case t: BinaryOpT =>
        val lhs = substitute(t.lhs, name, withTree)
        val rhs = substitute(t.rhs, name, withTree)
        new BinaryOpT(t.op, lhs, rhs) { val tpe = t.tpe }
      case t: LetT =>
        val value = substitute(t.value, name, withTree)
        val body = if (t.name.name != name) {
          substitute(t.body, name, withTree)
        } else {
          t.body
        }
        new LetT(t.name, value, body) { val tpe = t.tpe }
      case t: IfT =>
        val cond = substitute(t.cond, name, withTree)
        val thenp = substitute(t.thenp, name, withTree)
        val elsep = substitute(t.elsep, name, withTree)
        new IfT(cond, thenp, elsep) { val tpe = t.tpe }
      case t: ClosureT =>
        val paramsName = t.params map { _.name }
        if (paramsName.contains(name)) {
          t
        } else {
          val body = substitute(t.body, name, withTree)
          new ClosureT(t.params, body) { val tpe = t.tpe }
        }
      case t: CallT =>
        val fun = substitute(t.fun, name, withTree)
        val args = t.args map { substitute(_, name, withTree) }
        new CallT(fun, args) { val tpe = t.tpe }
    }
  }
}


