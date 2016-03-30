package calc

import org.scalajs.core.ir
import ir.{Trees => irt, Types => irtpe}

object TypeChecker {

  private final val nativeFunType = Map(
    "sin" -> FunctionType(1),
    "cos" -> FunctionType(1),
    "pow" -> FunctionType(2),
    "sqrt" -> FunctionType(1)
  )

  private def assertType(treeType: Type, expectedType : Type): Unit = {
    if(treeType != expectedType)
      throw new TypeCheckException(treeType, expectedType)
  }

  def typeCheck(tree: Tree, listId: Map[String, Type] = Map()): Type = {
    tree match {
      case Literal(value) => NumberType

      case BinaryOp(op, rhs, lhs) =>
        assertType(typeCheck(rhs, listId), NumberType)
        assertType(typeCheck(lhs, listId), NumberType)
        NumberType

      case Let(name, value, body) =>
        val letVal = typeCheck(value, listId)
        typeCheck(body, listId + (name.name -> letVal))

      case Ident(name) =>
        if(listId.contains(name)) listId(name)
        else throw new UnknownIdentException(name)

      case If(cond, thenp, elsep) =>
        assertType(typeCheck(cond, listId), NumberType)
        assertType(typeCheck(thenp, listId), typeCheck(elsep, listId))
        typeCheck(thenp, listId)

      case Closure(params, body) =>
        val typePar = params map ( x => (x.name, NumberType) )
        assertType(typeCheck(body, listId ++ typePar ), NumberType)
        FunctionType(params.length)

      case Call(Ident(name) , args)
        if (!(listId.contains(name)) && nativeFunType.contains(name)) =>
          args.map(x => assertType(typeCheck(x, listId), NumberType))
          assertType(nativeFunType(name), FunctionType(args.length))
          NumberType

      case Call(fun , args) =>
        args.map(x => assertType(typeCheck(x, listId), NumberType))
        assertType( typeCheck(fun, listId), FunctionType(args.length))
        NumberType
    }
  }
}
