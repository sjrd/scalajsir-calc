package calc

import org.scalajs.core.ir
import org.scalajs.core.ir.{Position, Trees => irt, Types => irtpe}
import ir.Definitions._
import calc.Typechecker.getScalaJSType

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  final val MainObjectFullName = "main.Main"

  type Scope = Map[String, TreeType]

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

    // typechecking:
    val typedTree = Typechecker.typecheck(tree)

    val body = compileExpr(typedTree)

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
  def compileExpr(tree: Tree, scope: Scope=Map.empty[String, TreeType]): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case Literal(value, _) =>
        irt.DoubleLiteral(value)

      case i@Ident(name, tp) => scope.get(name) match {
        // we have to cast parameters to Double, as they are always of type AnyType:
        case Some(t) if tp.isParam => irt.Unbox(irt.VarRef(irt.Ident(name))(irtpe.AnyType), 'D')
        case Some(t) => irt.VarRef(irt.Ident(name))(tp)
        case None    => throw new Exception(s"Not in scope: $name") // should never happen -- typechecker will spot it first
      }

      case BinaryOp(op, lhs, rhs, _) =>
        irt.BinaryOp(parseBinOp(op), compileExpr(lhs, scope), compileExpr(rhs, scope))

      case Let(ident, value, body, _) =>
        val newScope = scope + (ident.name -> value.tp)
        val valueC = compileExpr(value, scope)
        val bodyC = compileExpr(body, newScope)
        val varDefC = irt.VarDef(irt.Ident(ident.name), ident.tp, mutable = false, valueC)
        irt.Block(List(varDefC, bodyC))

      case If(cond, thenp, elsep, tp) =>
        // in this language, we could just optimize-out the if statement (condition is always a number)
        val condC  = evalIfCond(cond)
        val thenpC = compileExpr(thenp, scope)
        val elsepC = compileExpr(elsep, scope)
        irt.If(condC, thenpC, elsepC)(tp)

      case Closure(params, body, _) =>
        val captureParamsC = paramsFromScope(scope)
        val paramsC = paramsFromList(params)
        val bodyC = irt.Block(compileExpr(body, updateScopeWithParams(params, scope)))
        val captureValsC = varRefsFromScope(scope)
        irt.Closure(captureParamsC, paramsC, bodyC, captureValsC)

      case Call(fun, args, _) => fun match {
        case Ident(name, _) =>
          val argsC = args.map(compileExpr(_, scope))
          irt.Unbox(irt.JSFunctionApply(compileExpr(fun, scope), argsC), 'D')
        case _ => throw new Exception("Calling a non-function object.") // again: should never happen
      }

      case _ =>
        throw new Exception(s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }


  /* ------------------------------ HELPER METHODS ---------------------------------- */

  // converts the textual representation into the binary opcode from Trees
  private def parseBinOp(op: String): irt.BinaryOp.Code = op match {
    case "+" => irt.BinaryOp.Double_+
    case "-" => irt.BinaryOp.Double_-
    case "*" => irt.BinaryOp.Double_*
    case "/" => irt.BinaryOp.Double_/
    case "%" => irt.BinaryOp.Double_%
    case _   => throw new Exception(s"Unsupported binary operation: $op")
  }

  // if the condition has to be a number, we might as well calculate it in compile-time:
  private def evalIfCond(cond: Tree)(implicit pos: Position): irt.BooleanLiteral = cond match {
    case Literal(v, _) if v != 0 => irt.BooleanLiteral(value = true)
    case _                       => irt.BooleanLiteral(value = false) // this can only be a literal -- checked by TC
  }

  // transforms the scope into the list of capture params for closure:
  private def paramsFromScope(scope: Scope)(implicit pos: Position): List[irt.ParamDef] = {
    def tupleToParamDef(p: (String, TreeType)): irt.ParamDef =
      irt.ParamDef(irt.Ident(p._1), p._2, mutable = false, rest = false)

    scope.toList.map(tupleToParamDef)
  }

  // transforms the scope into the list of var-refs (that gets fed to a closure):
  private def varRefsFromScope(scope: Scope)(implicit pos: Position): List[irt.VarRef] = {
    def tupleToVarRef(p: (String, TreeType)): irt.VarRef = irt.VarRef(irt.Ident(p._1))(p._2)

    scope.toList.map(tupleToVarRef)
  }

  // transforms the param list into param list for closure:
  private def paramsFromList(params: List[Tree])(implicit pos: Position): List[irt.ParamDef] =
    params map {
      case Ident(name, tp) => irt.ParamDef(irt.Ident(name), tp, mutable = false, rest = false)
      case _ => throw new Exception(s"Non-identifier in argument position in function declaration.")
    }

  // updates the scope with closure's parameters:
  private def updateScopeWithParams(params: List[Tree], scope: Scope): Scope = {
    val paramTuples = params map {
      case Ident(name, tp) => (name, tp);
      case _ => throw new Exception("Unexpected form of parameter.")
    }
    val paramMap = paramTuples.toMap
    scope ++ paramMap  // in case of duplicates, m1 ++ m2 discards the entries from the first map
  }

}
