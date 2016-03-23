package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import scala.collection.immutable.Map
import java.util.UUID.randomUUID

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
    val body = compileExpr(tree)()
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

  var seed = 1
  def generateName():String = {
    seed += 1
    "generated_"+seed.toString
  }

  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree)(env:Map[String, irt.Tree] = Map(),typeEnv:Map[String, irtpe.Type] = Map()): irt.Tree = {
    // TODO
    implicit val pos = tree.pos

    tree match {
      case Literal(value) =>
        irt.DoubleLiteral(value)

      case BinaryOp(op, lhs, rhs) => {
        val jsop = op match {
          case "+" => irt.BinaryOp.Double_+
          case "-" => irt.BinaryOp.Double_-
          case "*" => irt.BinaryOp.Double_*
          case "/" => irt.BinaryOp.Double_/
        }
        irt.BinaryOp(jsop, compileExpr(lhs)(env, typeEnv), compileExpr(rhs)(env, typeEnv))
      }

      case Let(ident, value, body) => {
        val jsV = compileExpr(value)(env, typeEnv)
        val jsIdent = irt.Ident(ident.name)
        val jsId = irt.VarDef(jsIdent, jsV.tpe, false, jsV)
        irt.Block(List(jsId, compileExpr(body)(env+(ident.name -> jsV), typeEnv+(ident.name -> jsV.tpe))))
      }

      case Ident(n) => {
        try {
          typeEnv(n) match {
            case irtpe.DoubleType => irt.Unbox(irt.VarRef(irt.Ident(n))(typeEnv(n)),'D')
            case _ => irt.VarRef(irt.Ident(n))(typeEnv(n))
          }
        }catch{
          case e:Exception => irt.VarRef(irt.Ident(n)) (irtpe.DoubleType)
        }
      }

      case If(cond, thenp, elsep) => {
        val jsThenp = compileExpr(thenp)(env, typeEnv)
        val jsElsep = compileExpr(elsep)(env, typeEnv)
        val jsCond = compileExpr(cond)(env, typeEnv)
        val finalCond = irt.BinaryOp(irt.BinaryOp.!==, irt.Unbox(jsCond,'D'), irt.DoubleLiteral(0.0))
        if (jsElsep.tpe != jsElsep.tpe){
          throw new Exception(s"The then and also phase in the conditional statement must bse same type")
        }
        irt.If(finalCond, jsThenp, jsElsep)(jsThenp.tpe)
      }

      case Closure(idList, body) => {
        val captureList = findCapture(tree).toList
        val captureParam = captureList.map(s => irt.ParamDef(irt.Ident(s), typeEnv(s), false, false))
        val params = idList.map(s => irt.ParamDef(irt.Ident(s.name), irtpe.AnyType, false, false))
        val newTypeEnv = idList.foldLeft(typeEnv) ((env:Map[String,irtpe.Type], s:Ident) => env+(s.name -> irtpe.DoubleType))
        irt.Closure(captureParam, params, compileExpr(body)(env,newTypeEnv), captureList.map(s => env(s)))
      }

      case Call(f, args) => {
        val jsFunction = compileExpr(f)(env, typeEnv)
        irt.Unbox(irt.JSFunctionApply(jsFunction,args.map(s => irt.Unbox(compileExpr(s)(env, typeEnv),'D'))),'D')
      }


      case _ => throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
