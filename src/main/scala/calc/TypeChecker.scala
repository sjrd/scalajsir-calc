package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}
import ir.Definitions._
import collection.immutable.Map

sealed abstract class Type

case object Double extends Type

case class Fun(arg: Type, body: Type) extends Type

/**
  * Created by ivan on 16/3/23.
  * A type checker and infer about types
  */
object TypeChecker {
  private val idToType = Map[String, Type]()
  def typeInfer(tree: Tree)(implicit typeEnv:Map[String, Type]): Type ={
    tree match {
      case Literal(value) => Double
      case Ident(name) => typeEnv(name)
      case BinaryOp(op, lhs, rhs) => {
          require(typeInfer(lhs) == typeInfer(rhs))
          typeInfer(lhs)
      }
      case Let(name, value, body) => ??? //typeInfer(body)(idToType + (name.name -> typeInfer(value)))
      case Closure(params, body) => ???
      case Call(fun, args) => ???
      case If(cond, thenp, elsep) => ???
    }
  }
}
