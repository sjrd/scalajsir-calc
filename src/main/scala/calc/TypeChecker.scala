package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import collection.immutable.Map

sealed abstract class TreeType

case object DoubleType extends TreeType

case class FunType(numOfArgs: Int) extends TreeType

/**
  * Created by ivan on 16/3/23.
  * A type checker and infer about types
  */
object TypeChecker {
  def typeInfer(tree: Tree, typeEnv: Map[String, TreeType] = Map()): TreeType ={
    tree match {
      case Literal(value) => DoubleType
      case Ident(name) => typeEnv(name)
      case BinaryOp(op, lhs, rhs) => {
        val ltype = typeInfer(lhs, typeEnv)
        val rtype = typeInfer(rhs, typeEnv)
        if (ltype == rtype) DoubleType
        else throw new Exception("Binary operands does not match!")
      }
      case Let(n, value, body) => {
        val valueType = typeInfer(value, typeEnv)
        typeInfer(body, typeEnv + (n.name -> valueType))
      }
      case Closure(params, body) =>
        FunType(params.length)
      case Call(fun, args) => {
        val funType = typeInfer(fun, typeEnv)
        if (args.length == funType.asInstanceOf[FunType].numOfArgs) DoubleType
        else throw new Exception("Function call does not match!")
      }
      case If(cond, thenp, elsep) => {
        if (typeInfer(cond, typeEnv) == DoubleType){
          val thenType = typeInfer(thenp, typeEnv)
          val elseType = typeInfer(elsep, typeEnv)
          if (thenType == elseType) thenType
          else throw new Exception("Condition statemet's two phases' type does not match!")
        }
        else throw new Exception("Condition should only be number!")

      }
    }
  }
}
