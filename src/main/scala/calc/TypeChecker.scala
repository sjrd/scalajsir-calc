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
  val mathFunctions = Set("sin", "log", "cos")
  def typeCheck(tree: Tree, typeEnv: Map[String, TreeType] = Map()): TreeType ={
    tree match {
      case Literal(value) => DoubleType
      case Ident(name) =>
        try{
          typeEnv(name)
        }catch{
          case e:Exception => if (mathFunctions.contains(name))
            FunType(1)
          else throw new Exception(s"Identifier ${name} is not in the scope")
        }
      case BinaryOp(op, lhs, rhs) => {
        val ltype = typeCheck(lhs, typeEnv)
        val rtype = typeCheck(rhs, typeEnv)
        if (ltype == DoubleType && rtype == DoubleType) DoubleType
        else throw new Exception("Binary operands does not match!")
      }
      case Let(n, value, body) => {
        val valueType = typeCheck(value, typeEnv)
        typeCheck(body, typeEnv + (n.name -> valueType))
      }
      case Closure(params, body) =>
        assert(typeCheck(body, typeEnv ++ params.map(s => (s.name, DoubleType))) == DoubleType)
        FunType(params.length)
      case Call(fun, args) => {
        val funType = typeCheck(fun, typeEnv)
        if (!funType.isInstanceOf[FunType])
          throw new Exception(s"You could only apply on a function!")
        args.map(s => assert(typeCheck(s, typeEnv) == DoubleType))
        if (args.length == funType.asInstanceOf[FunType].numOfArgs) DoubleType
        else throw new Exception("Function call does not match!")
      }
      case If(cond, thenp, elsep) => {
        if (typeCheck(cond, typeEnv) == DoubleType){
          val thenType = typeCheck(thenp, typeEnv)
          val elseType = typeCheck(elsep, typeEnv)
          if (thenType == elseType) thenType
          else throw new Exception("Condition statemet's two phases' type does not match!")
        }
        else throw new Exception("Condition should only be number!")

      }
    }
  }
}
