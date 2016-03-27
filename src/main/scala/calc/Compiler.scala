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
        case Some(t) if tp.isParam =>
          irt.Unbox(irt.VarRef(irt.Ident(name))(irtpe.AnyType), 'D')

        case Some(t) => irt.VarRef(irt.Ident(name))(tp)

        // should never happen, typechecker will spot it first:
        case None => throw new Exception(s"Not in scope: $name")
      }

      case BinaryOp(op, lhs, rhs, _) =>
        irt.BinaryOp(parseBinOp(op),
            compileExpr(lhs, scope), compileExpr(rhs, scope))

      case Let(ident, value, body, _) =>
        val newScope = scope + (ident.name -> value.tp)
        val valueC   = compileExpr(value, scope)
        val bodyC    = compileExpr(body, newScope)
        val varDefC  = irt.VarDef(irt.Ident(ident.name),
            ident.tp, mutable = false, valueC)

        irt.Block(List(varDefC, bodyC))

      case If(cond, thenp, elsep, tp) =>
        val condC  = processIfCond(cond, scope)
        val thenpC = compileExpr(thenp, scope)
        val elsepC = compileExpr(elsep, scope)

        irt.If(condC, thenpC, elsepC)(tp)

      case cl@Closure(params, body, _) =>
        val cpts           = captures(cl, scope)
        val captureParamsC = cpts._1
        val captureValsC   = cpts._2
        val paramsC        = paramsFromList(params)
        val bodyC          = irt.Block(
            compileExpr(body, updateScopeWithParams(params, scope)))

        irt.Closure(captureParamsC, paramsC, bodyC, captureValsC)

      case Call(fun, args, _) => fun match {
        case Ident(name, _) =>
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
  private def processIfCond(cond: Tree, scope: Scope)
                        (implicit pos: Position): irt.BinaryOp = {
    val condC = compileExpr(cond, scope)
    val zeroC = irt.DoubleLiteral(0.0)
    irt.BinaryOp(irt.BinaryOp.Num_!=, condC, zeroC)
  }

  /** Extracts all the free variables from the closure body. */
  private def freeVars(closure: Closure): Set[String] = {
    val ps = closure.params

    def freeVarsAux(body: Tree): Set[String] = body match {
      case id@Ident(name, _) if !ps.contains(id) =>
        Set(id.name)

      case Ident(_, _) =>
        Set.empty

      case Literal(_, _) =>
        Set.empty

      case BinaryOp(_, lhs, rhs, _) =>
        freeVarsAux(lhs) ++ freeVarsAux(rhs)

      case Let(_, value, b, _) =>
        freeVarsAux(value) ++ freeVarsAux(b)

      case If(cond, thenp, elsep, _) =>
        freeVarsAux(cond) ++ freeVarsAux(thenp) ++ freeVarsAux(elsep)

      case Call(fun, args, _) =>
        freeVarsAux(fun) ++ args.flatMap(freeVarsAux).toSet

      case Closure(params, b, _)  =>
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
  private def captures(closure: Closure, scope: Scope)
                      (implicit pos: Position): (List[irt.ParamDef], List[irt.VarRef]) = {
    val freeVs     = freeVars(closure)
    val isFree     = (p: (String, _)) => freeVs.contains(p._1)
    val cs         = scope.toList.filter(isFree)
    val cParamDefs = captureParams(cs, scope)
    val cVarRefs   = captureRefs(cs, scope)

    (cParamDefs, cVarRefs)
  }

  /** Transforms the param list into param list for closure. */
  private def paramsFromList(params: List[Tree])
                            (implicit pos: Position): List[irt.ParamDef] =
    params map {
      case Ident(name, tp) =>
        irt.ParamDef(irt.Ident(name), tp, mutable = false, rest = false)

      case _ =>
        throw new Exception(s"Non-identifier in argument position in function declaration.")
    }

  /** Updates the scope with closure's parameters. */
  private def updateScopeWithParams(params: List[Tree], scope: Scope): Scope = {
    val paramTuples = params map {
      case Ident(name, tp) => (name, tp)
      case _               => throw new Exception("Unexpected form of parameter.")
    }

    val paramMap = paramTuples.toMap

    scope ++ paramMap  // in case of duplicates, m1 ++ m2 discards the entries from the first map
  }

}
