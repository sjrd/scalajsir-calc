package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import scala.collection.immutable.Map
import TypeChecker._

/** Main compiler.
 *
 *  You have to implement the method `compileExpr`.
 */
object Compiler {
  final val MainObjectFullName = "main.Main"

  private final val MainClassFullName = MainObjectFullName + "$"

  //implicit val defaultEnv = Map[String, irt.Tree] ()
  //implicit val defaultTypeEnv = Map[String, irtpe.Type]()
  //implicit val defaultParams = Set[String]()
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



  def findCapture(tree: Tree): Set[String] = {
    tree match {
      case _:Literal => Set()
      case BinaryOp(_, l, r) => findCapture(l) ++ findCapture(r)
      case Let(i, v, body) => findCapture(body) ++ findCapture(v) - i.name
      case Ident(n) => Set(n)
      case If(cond, thenp, elsep) => findCapture(cond) ++ findCapture(thenp) ++ findCapture(elsep)
      case Closure(idents, body) => findCapture(body) -- idents.flatMap((x:Tree) => findCapture(x)).toSet
      case Call(f, args) => findCapture(f) ++ args.flatMap((x:Tree) => findCapture(x)).toSet
    }
  }


  def compileExpr(tree: Tree):irt.Tree ={
    implicit val pos = tree.pos
    typeCheck(tree)
    implicit val mathModule = irt.LoadModule(irtpe.ClassType("jl_Math$"))
    compileExpr(tree, Map())
  }


  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree, typeEnv: Map[String, irtpe.Type])(implicit mathModule: irt.Tree): irt.Tree = {
    implicit val pos = tree.pos
    val functionTypes:Map[String, Int] = Map("sin" -> 1,
      "cos" -> 1,
      "tan" -> 1,
      "log" -> 1,
      "pow" -> 2,
      "abs" -> 1,
      "max" -> 2,
      "min" -> 2)
    tree match {
      case Literal(value) =>
        irt.DoubleLiteral(value)

      case BinaryOp(op, lhs, rhs) =>
        val jsop = op match {
          case "+" => irt.BinaryOp.Double_+
          case "-" => irt.BinaryOp.Double_-
          case "*" => irt.BinaryOp.Double_*
          case "/" => irt.BinaryOp.Double_/
        }
        irt.BinaryOp(jsop, compileExpr(lhs, typeEnv), compileExpr(rhs, typeEnv))

      case Let(ident, value, body) =>
        val jsV = compileExpr(value, typeEnv)
        val jsIdent = irt.Ident(ident.name)
        val jsId = irt.VarDef(jsIdent, jsV.tpe, mutable = false, jsV)
        irt.Block(List(jsId, compileExpr(body, typeEnv + (ident.name -> jsV.tpe))))

      case Ident(n) =>
        try {
          irt.VarRef(irt.Ident(n))(typeEnv(n))
        }catch{
          case e:Exception => throw new Exception("No such identitier!")
        }

      case If(cond, thenp, elsep) =>
        val jsThenp = compileExpr(thenp, typeEnv)
        val jsElsep = compileExpr(elsep, typeEnv)
        val jsCond = compileExpr(cond, typeEnv)
        val finalCond = irt.BinaryOp(irt.BinaryOp.!==, jsCond, irt.DoubleLiteral(0.0))
        if (jsElsep.tpe != jsElsep.tpe){
          throw new Exception(s"The then and also phase in the conditional statement must bse same type")
        }
        irt.If(finalCond, jsThenp, jsElsep)(jsThenp.tpe)

      case Closure(idList, body) =>
        val captureList = findCapture(tree).toList
        val captureParam = captureList.map(s =>
          irt.ParamDef(irt.Ident(s), typeEnv(s), mutable = false, rest = false))
        val params = idList.map(s =>
          irt.ParamDef(irt.Ident("$$"+s.name), irtpe.AnyType, mutable = false, rest = false))
        val newTypeEnv = idList.foldLeft(typeEnv)((env: Map[String, irtpe.Type], s:Ident) => env+(s.name -> irtpe.DoubleType))
        val unboxParams = idList.map(s =>
          irt.VarDef(irt.Ident(s.name), irtpe.DoubleType, mutable = false,
            irt.Unbox(irt.VarRef(irt.Ident("$$"+s.name))(irtpe.AnyType), 'D')))
        val bl = irt.Block(unboxParams ++ List(compileExpr(body, newTypeEnv)))
        irt.Closure(captureParam, params, bl, captureList.map(s => irt.VarRef(irt.Ident(s))(typeEnv(s))))

      case Call(f, args) =>
        try{
          val jsFunction = compileExpr(f, typeEnv)
          irt.Unbox(irt.JSFunctionApply(jsFunction,args.map(s => compileExpr(s, typeEnv))), 'D')
        }catch{
          case e:Exception=> {
            try {
              val funcName = f.asInstanceOf[Ident].name
              val identName = funcName + "__D" * functionTypes(funcName) + "__D"
              irt.Apply(mathModule, irt.Ident(identName),
                args.map(s => compileExpr(s, typeEnv)))(irtpe.DoubleType)
            }catch{
              case e:Exception => throw new Exception("The called function is not defined or supported function!")
            }
          }
        }


      case _ => throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
