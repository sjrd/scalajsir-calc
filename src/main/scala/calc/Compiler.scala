package calc

import org.scalajs.core.ir
import org.scalajs.core.ir.{Trees => irt, Types => irtpe, Position}
import ir.Definitions._

import calc.Typechecker.getScalaJSType

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  final val MainObjectFullName = "main.Main"

  type Scope = Map[String, Tree]

  var varDefMap: Map[String, irt.VarDef] = Map.empty[String, irt.VarDef]

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

  private def parseBinOp(op: String): irt.BinaryOp.Code = op match {
    case "+" => irt.BinaryOp.Double_+
    case "-" => irt.BinaryOp.Double_-
    case "*" => irt.BinaryOp.Double_*
    case "/" => irt.BinaryOp.Double_/
    case "%" => irt.BinaryOp.Double_%
    case _   => throw new Exception(s"Unsupported binary operation: $op")
  }

  private def evalIfCond(cond: Tree)(implicit pos: Position): irt.BooleanLiteral = cond match {
    case Literal(v, _) if v != 0 => irt.BooleanLiteral(true)
    case _                       => irt.BooleanLiteral(false) // this can only be a literal -- checked by TC
  }

  private def paramToParamdef(p: Ident)(implicit pos: Position): irt.ParamDef =
    irt.ParamDef(irt.Ident(p.name), p.tp, mutable = false, rest = false)

  val paramdefToTuple = (p: irt.ParamDef) => (p.name.name, p)

  private def processParams(params: List[Ident])(implicit pos: Position): List[irt.ParamDef] =
    params map paramToParamdef

  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree, scope: Scope=Map.empty[String, Tree]): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case Literal(value, _) =>
        irt.DoubleLiteral(value)

      case i@Ident(name, tp) => scope.get(name) match {
        case Some(t) => irt.VarRef(irt.Ident(name))(tp) // compileExpr(t, scope)
        case None    => throw new Exception(s"Not in scope: $name")
      }

      case BinaryOp(op, lhs, rhs, _) =>
        irt.BinaryOp(parseBinOp(op), compileExpr(lhs, scope), compileExpr(rhs, scope))

      case Let(ident, value, body, _) =>
        irt.Block(List(irt.VarDef(
          irt.Ident(ident.name), ident.tp, mutable = false, compileExpr(value)),
          compileExpr(body, scope + (ident.name -> value))))

      case If(cond, thenp, elsep, tp) =>
        val condC  = evalIfCond(cond)
        val thenpC = compileExpr(thenp, scope)
        val elsepC = compileExpr(elsep, scope)
        irt.If(condC, thenpC, elsepC)(tp)

      case Closure(params, body, _) =>
        ???

      case Call(fun, args, _) =>
        ???

      case _ =>
        throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
