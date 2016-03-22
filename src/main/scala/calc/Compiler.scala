package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import scala.collection.mutable.Map

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

  var idToType:Map[String, irtpe.Type] = Map()
  var idToValue:Map[String, irt.Tree] = Map()


  def findCapture(tree: Tree): Set[String] = {
    tree match {
      case _:Literal => Set()
      case BinaryOp(_, l, r) => findCapture(l) ++ findCapture(r)
      case Let(i, v, body) => findCapture(body) ++ findCapture(v) - i.name
      case Ident(n) => Set(n)
      case If(cond, thenp, elsep) => findCapture(cond) ++ findCapture(thenp) ++ findCapture(elsep)
      case Closure(idents, body) => findCapture(body) -- idents.toSet.flatMap((x:Tree) => findCapture(x))
      case Call(f, args) => findCapture(f) ++ args.toSet.flatMap((x:Tree) => findCapture(x))
    }
  }

  def compileClosure(tree: Closure, ident: String): irt.Tree ={
    val captureSet = findCapture(tree) - ident
    val captureParam = captureSet.map(s => irt.ParamDef(irt.Ident(s)(tree.pos), idToType(s), false, false)(tree.pos)).toList
    val params = tree.params.map(s => irt.ParamDef(irt.Ident(s.name)(tree.pos), irtpe.AnyType, false, false)(tree.pos))
    irt.Closure(captureParam, params, compileExpr(tree.body), captureSet.map(s => idToValue(s)).toList)(tree.pos)
  }


  /** Compile an expression tree into an IR `Tree`, which is an expression
   *  that evaluates to the result of the tree.
   *
   *  This is the main method you have to implement.
   */
  def compileExpr(tree: Tree): irt.Tree = {
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
        irt.BinaryOp(jsop, irt.Unbox(compileExpr(lhs),'D'), irt.Unbox(compileExpr(rhs), 'D'))
      }

      case Let(ident, value, body) => {
        var jsV:irt.Tree = if (value.isInstanceOf[Closure]){
          compileClosure(value.asInstanceOf[Closure], ident.name)
        } else{
          compileExpr(value)
        }
        val jsIdent = irt.Ident(ident.name)
        val jsId = irt.VarDef(jsIdent, jsV.tpe, false, jsV)
        idToType += (ident.name -> jsV.tpe)
        idToValue += (ident.name -> jsV)
        irt.Block(List(jsId, compileExpr(body)))
      }

      case Ident(n) => {
        try {
          irt.VarRef(irt.Ident(n))(idToType(n))
        }catch{
          case e:Exception => irt.VarRef(irt.Ident(n))(irtpe.AnyType)
          //throw new Exception(s"The variable ${n} is not in the scope")
        }
      }

      case If(cond, thenp, elsep) => {
        val jsThenp = compileExpr(thenp)
        val jsElsep = compileExpr(elsep)
        val jsCond = compileExpr(cond)
        val finalCond = irt.BinaryOp(irt.BinaryOp.!==, irt.Unbox(jsCond,'D'), irt.DoubleLiteral(0.0))
        if (jsElsep.tpe != jsElsep.tpe){
          throw new Exception(s"The then and also phase in the conditional statement must bse same type")
        }
        irt.If(finalCond, jsThenp, jsElsep)(jsThenp.tpe)
      }

      case Closure(idList, body) => {
        /*val capturePara = idList.map(s => s match{
          case Ident(n) => irt.ParamDef(irt.Ident(n), irtpe.AnyType, false, false)
          case _ => throw new Exception("Function def error!")
        })*/
        val captureSet = findCapture(tree)
        val captureParam = captureSet.map(s => irt.ParamDef(irt.Ident(s), idToType(s), false, false)).toList
        val params = idList.map(s => irt.ParamDef(irt.Ident(s.name), irtpe.AnyType, false, false))
        irt.Closure(captureParam, params, compileExpr(body), captureSet.map(s => idToValue(s)).toList)
      }

      case Call(f, args) => {
        val jsFunction = compileExpr(f)
        /*val jsClass = irt.GetClass(jsFunction)
        val fName = f match {
          case Ident(n) => n
          case _ => throw new Exception("TO")
        }*/
        //irt.Apply(irt.This()(irtpe.ClassType(MainClassFullName)), irt.Ident(fName), args.map(s => compileExpr(s)))(irtpe.DoubleType)
        irt.Unbox(irt.JSFunctionApply(jsFunction,args.map(s => compileExpr(s))),'D')
      }


      case _ => throw new Exception(
            s"Cannot yet compile a tree of class ${tree.getClass}")
    }
  }
}
