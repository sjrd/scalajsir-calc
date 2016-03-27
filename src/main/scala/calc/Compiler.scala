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
  def compileExpr(tree: TypedTree, scope: Scope=Map.empty[String, TreeType]): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case LiteralT(value, _) =>
        irt.DoubleLiteral(value)

      case i@IdentT(name, tp) => scope.get(name) match {
        case Some(t) => irt.VarRef(irt.Ident(name))(tp)

        // should never happen, typechecker will spot it first:
        case None => throw new Exception(s"Not in scope: $name")
      }

      case BinaryOpT(op, lhs, rhs, _) =>
        irt.BinaryOp(parseBinOp(op),
            compileExpr(lhs, scope), compileExpr(rhs, scope))

      case LetT(ident, value, body, _) =>
        val newScope = scope + (ident.name -> value.tp)
        val valueC   = compileExpr(value, scope)
        val bodyC    = compileExpr(body, newScope)
        val varDefC  = irt.VarDef(irt.Ident(ident.name),
            ident.tp, mutable = false, valueC)

        irt.Block(List(varDefC, bodyC))

      case IfT(cond, thenp, elsep, tp) =>
        val condC  = processIfCond(cond, scope)
        val thenpC = compileExpr(thenp, scope)
        val elsepC = compileExpr(elsep, scope)

        irt.If(condC, thenpC, elsepC)(tp)

      case cl@ClosureT(params, body, _) =>
        val cpts           = captures(cl, scope)
        val captureParamsC = cpts._1
        val captureValsC   = cpts._2
        val paramsC        = paramsFromList(params)
        val bodyC          = irt.Block(
            localParamVars(params),
            compileExpr(body, updateScopeWithParams(params, scope)))

        irt.Closure(captureParamsC, paramsC, bodyC, captureValsC)

      case CallT(fun, args, _) => fun match {
        case IdentT(name, _) =>
          val argsC = args.map(compileExpr(_, scope))

          irt.Unbox(irt.JSFunctionApply(compileExpr(fun, scope), argsC), 'D')

        // again: should never happen:
        case _ => throw new Exception("Calling a non-function object.")
      }

      case _ =>
        throw new Exception(s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }

  /* ------------------------------ HELPER METHODS ---------------------------------- */

  /** Converts the textual representation into the binary opcode from Trees. */
  private def parseBinOp(op: String): irt.BinaryOp.Code = op match {
    case "+" => irt.BinaryOp.Double_+
    case "-" => irt.BinaryOp.Double_-
    case "*" => irt.BinaryOp.Double_*
    case "/" => irt.BinaryOp.Double_/
    case "%" => irt.BinaryOp.Double_%
    case _   => throw new Exception(s"Unsupported binary operation: $op")
  }

  /** Creates a comparison-with-zero to evaluate the if condition.
    * (The condition is true if it is non-zero). */
  private def processIfCond(cond: TypedTree, scope: Scope)
                        (implicit pos: Position): irt.BinaryOp = {
    val condC = compileExpr(cond, scope)
    val zeroC = irt.DoubleLiteral(0.0)
    irt.BinaryOp(irt.BinaryOp.Num_!=, condC, zeroC)
  }

  /** Extracts all the free variables from the closure body. */
  private def freeVars(closure: ClosureT): Set[String] = {
    val ps = closure.params

    def freeVarsAux(body: TypedTree): Set[String] = body match {
      case id@IdentT(name, _) if !ps.contains(id) =>
        Set(id.name)

      case IdentT(_, _) =>
        Set.empty

      case LiteralT(_, _) =>
        Set.empty

      case BinaryOpT(_, lhs, rhs, _) =>
        freeVarsAux(lhs) ++ freeVarsAux(rhs)

      case LetT(_, value, b, _) =>
        freeVarsAux(value) ++ freeVarsAux(b)

      case IfT(cond, thenp, elsep, _) =>
        freeVarsAux(cond) ++ freeVarsAux(thenp) ++ freeVarsAux(elsep)

      case CallT(fun, args, _) =>
        freeVarsAux(fun) ++ args.flatMap(freeVarsAux).toSet

      case ClosureT(params, b, _)  =>
        params.flatMap(freeVarsAux).toSet ++ freeVarsAux(b)
    }

    freeVarsAux(closure.body)
  }

  private def captureParams(captures: List[(String, TreeType)], scope: Scope)
                           (implicit pos: Position): List[irt.ParamDef] = {
    captures map { p =>
      irt.ParamDef(irt.Ident(p._1), p._2,
          mutable = false, rest = false)
    }
  }

  private def captureRefs(captures: List[(String, TreeType)], scope: Scope)
                         (implicit pos: Position): List[irt.VarRef] = {
    captures map { p =>
      irt.VarRef(irt.Ident(p._1))(p._2)
    }
  }

  /** All the capture params and their corresponding VarRefs for the closure.
    * It returns an awkward tuple to avoid finding the free variables 2 times. */
  private def captures(closure: ClosureT, scope: Scope)
                      (implicit pos: Position): (List[irt.ParamDef], List[irt.VarRef]) = {
    val freeVs     = freeVars(closure)
    val isFree     = (p: (String, _)) => freeVs.contains(p._1)
    val cs         = scope.toList.filter(isFree)
    val cParamDefs = captureParams(cs, scope)
    val cVarRefs   = captureRefs(cs, scope)

    (cParamDefs, cVarRefs)
  }

  /** Mangles the name according to the type. */
  private def mangleName(name: String, tp: TreeType): String =
    name ++ "__" ++ tp.typeSuff

  /** Transforms the param list into param list for closure.
    * Also, mangles the parameter names. */
  private def paramsFromList(params: List[TypedTree])
                            (implicit pos: Position): List[irt.ParamDef] = {
    params map {
      case IdentT(name, tp) =>
        irt.ParamDef(
          irt.Ident(mangleName(name, tp)),
          irtpe.AnyType, // all params have to be AnyType
          mutable = false, rest = false)

      case _ =>
        throw new Exception(s"Non-identifier in argument position in function declaration.")
    }
  }

  /** Produces the list of local variable declarations */
  private def localParamVars(params: List[IdentT])
                            (implicit pos: Position): irt.Tree = {
    def localVar(param: IdentT): irt.VarDef = param match {
      case IdentT(name, tp) =>
        val typeCode = tp.typeSuff.charAt(0)
        val nameM    = mangleName(name, tp)
      val varRef   = irt.Unbox(irt.VarRef(irt.Ident(nameM))(irtpe.AnyType), typeCode)
        val newIdent = irt.Ident(name)
        irt.VarDef(newIdent, tp, mutable = false, varRef)
    }

    irt.Block(params.map(localVar))
  }

  /** Updates the scope with closure's parameters. */
  private def updateScopeWithParams(params: List[TypedTree], scope: Scope): Scope = {
    val paramTuples = params map {
      case IdentT(name, tp) => (name, tp)
      case _               => throw new Exception("Unexpected form of parameter.")
    }

    val paramMap = paramTuples.toMap

    scope ++ paramMap  // in case of duplicates, m1 ++ m2 discards the entries from the first map
  }

}
