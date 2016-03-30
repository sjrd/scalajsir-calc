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

  private final val nativeFun = Map(
    "sin" -> "sin__D__D",
    "cos" -> "cos__D__D",
    "sqrt" -> "sqrt__D__D",
    "pow" -> "pow__D__D__D"
  )

  private final val mathClass =
    irtpe.ClassType(encodeClassName("java.lang.Math$"))

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

  def findFreeVar(tree: Tree, listId: Set[String]): Set[String]= {
    implicit val pos = tree.pos

    tree match {
      case Literal(value) => Set()

      case BinaryOp(op, rhs, lhs) =>
        findFreeVar(rhs, listId) ++ findFreeVar(lhs, listId)

      case Let(name, value, body) =>
        findFreeVar(value, listId) ++ findFreeVar(body, listId + name.name)

      case Ident(name) =>
        if(listId.contains(name)) Set()
        else Set(name)

      case If(cond, thenp, elsep) =>
        findFreeVar(cond, listId)  ++
        findFreeVar(thenp, listId) ++
        findFreeVar(elsep, listId)

      case Closure(params, body) =>
        val typePar = params map ( x => x.name )
        findFreeVar(body, listId ++ typePar)

      case Call(Ident(name) , args)
        if (!(listId.contains(name)) && nativeFun.contains(name)) =>
         args.map( x => findFreeVar(x, listId) ).reduce( (x,y) => x++y ).toSet

      case Call(fun , args) =>
        val argSet = args.map( x => findFreeVar(x, listId) )
          .reduce( (x, y) => x ++ y )
          .toSet
        argSet ++ findFreeVar(fun, listId)
    }
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
        if(listId.contains(name)) irt.VarRef(irt.Ident(name))(listId(name))
        else throw new Exception(s"Identifier ${name} not defined")

      case If(cond, thenp, elsep) =>
        val ifVal = compileExpr(cond, listId)
        irt.If(
          irt.BinaryOp(Num_!=, ifVal, irt.DoubleLiteral(0)),
          compileExpr(thenp, listId),
          compileExpr(elsep, listId)
        )(irtpe.DoubleType)

      case Closure(params, body) =>
        //Argument unboxing at the start of the function
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

        //The body of the closure
        val typePar = params map ( x => (x.name, irtpe.DoubleType) )
        val bodyClosure = compileExpr(body, listId ++ typePar)
        val blockClosure = irt.Block(stVar ++ List(bodyClosure))

        //Free variable discovery and capture list
        var paramSet = (params map( x => x.name )).toSet
        val capList = findFreeVar(body, paramSet).toList
        val capDef = capList.map { x => irt.ParamDef(irt.Ident(x),
          listId(x), false, false) }
        val capVal = capList.map
          { case x => irt.VarRef(irt.Ident(x))(listId(x)) }

        irt.Closure(capDef, listPar, blockClosure, capVal)

      case Call(Ident(name) , args)
        if (!(listId.contains(name)) && nativeFun.contains(name)) =>
          val argList = args.map(x => compileExpr(x,listId))
          val mathModule = irt.LoadModule(mathClass)
          val fun = irt.Ident(nativeFun(name))
          irt.Apply(mathModule, fun, argList)(irtpe.DoubleType)

      case Call(fun , args) =>
        val argList = args.map(x => compileExpr(x, listId))
        val callTree = irt.JSFunctionApply(compileExpr(fun, listId), argList)
        irt.Unbox(callTree,'D')

      case _ =>
        throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
