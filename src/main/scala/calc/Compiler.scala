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

  import irt.BinaryOp._
  private final val binaryOpMap = Map("+" -> Double_+, "-" -> Double_-,
      "*" -> Double_*, "/" -> Double_/)

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
  def compileExpr(tree: Tree, listId: Map[String,irtpe.Type] = Map()): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case Literal(value) =>
        irt.DoubleLiteral(value)

      case BinaryOp(op, rhs, lhs) =>
        val right = compileExpr(rhs, listId)
        val left  = compileExpr(lhs, listId)
        irt.BinaryOp(binaryOpMap(op), right, left)

      case Let(name, value, body) =>
        val irtId = irt.Ident(name.name)
        val letVal = compileExpr(value, listId)
        irt.Block(List(
          irt.VarDef(irtId, letVal.tpe, false, letVal),
          compileExpr(body, listId + (name.name -> letVal.tpe)))
        )

      case Ident(name) =>
        listId.contains(name) match {
          case true  => irt.VarRef(irt.Ident(name))(listId(name))
          case false => throw new Exception(s"Identifier ${name} not defined")
        }

      case If(cond, thenp, elsep) =>
        val ifVal = compileExpr(cond, listId)
        irt.If(
          irt.BinaryOp(Num_!=, ifVal, irt.DoubleLiteral(0)),
          compileExpr(thenp, listId),
          compileExpr(elsep, listId)
        )(irtpe.DoubleType)

      case Closure(params, body) =>
        val typePar = params map ( x => (x.name, irtpe.DoubleType))
        val listPar = params map { x =>
                        irt.ParamDef(irt.Ident("_" + x.name), irtpe.AnyType,
                          false, false)
                      }

        val stVar = params map { x =>
                      irt.VarDef(irt.Ident(x.name), irtpe.DoubleType, false,
                        irt.Unbox(irt.VarRef(irt.Ident("_" + x.name))
                          (irtpe.AnyType), 'D')
                      )
                    }

        val bl = irt.Block(stVar ++ List(compileExpr(body, listId ++ typePar)))
        irt.Closure(List(), listPar, bl, List())

      case Call(fun , args) =>
        val argList = args.map(x => compileExpr(x,listId))
        val callTree = irt.JSFunctionApply(compileExpr(fun, listId), argList)
        irt.Unbox(callTree,'D')

      case _ =>
        throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
