package calc

import org.scalajs.core.ir.{Types => irtpe, Position}

/** A simple typechecker.
 *
 * Checks whether the types match or are otherwise appropriate (e.g. in the if's condition).
 * Annotates AST with types, so that they can be used during compilation.
 *
  * TODO group errors and fail as late as possible, possibly without an exception (requires some type-work)
 */

object Typechecker {

  class TypecheckerException(msg: String) extends Exception(msg)

  type TypeScope = Map[String, TreeType]

  implicit def getScalaJSType(tp: TreeType): irtpe.Type = tp match {
    case DoubleType      => irtpe.DoubleType
    case ParamType       => irtpe.DoubleType
    case FunctionType(_) => irtpe.AnyType
    case NoType          => irtpe.NoType
  }

  def getType(tree: Tree, scope: TypeScope)
             (implicit pos: Position): TypedTree =  tree match {
    case Literal(x)    => LiteralT(x, DoubleType)
    case b:      BinaryOp => checkBinop(b, scope)
    case id:     Ident    => checkIdent(id, scope)
    case let:    Let      => checkLet(let, scope)
    case ifExpr: If       => checkIf(ifExpr, scope)
    case call:   Call     => checkCall(call, scope)
    case cl:     Closure  => checkClosure(cl, scope)
  }

  def typecheck(tree: Tree)(implicit pos: Position): TypedTree = {
    getType(tree, Map.empty[String, TreeType])
  }

  private def checkBinop(binop: BinaryOp, scope: TypeScope)
                        (implicit pos: Position): TypedTree = {
    val lhsTyped = getType(binop.lhs, scope)
    val rhsTyped = getType(binop.rhs, scope)
    val lType    = lhsTyped.tp
    val rType    = rhsTyped.tp

    if (!lType.isFunction && !rType.isFunction)
      BinaryOpT(binop.op, lhsTyped, rhsTyped, lType)
    else
      fail(binop.pos, s"Incompatible operand types for ${binop.op}: $lType, $rType")
  }

  private def checkIdent(ident: Ident, scope: TypeScope)
                        (implicit pos: Position): TypedTree =
    scope.get(ident.name) match {
      case Some(t) => IdentT(ident.name, t)
      case None    => fail(pos, s"Not in scope: ${ident.name}")
    }

  private def checkIf(ifExpr: If, scope: TypeScope)
                     (implicit pos: Position): TypedTree = ifExpr match {
    case If(cond, thenp, elsep) =>
      val condTyped = getType(cond, scope)
      val thenTyped = getType(thenp, scope)
      val elseTyped = getType(elsep, scope)
      val tType     = thenTyped.tp
      val eType     = elseTyped.tp

      if (tType != eType)
        fail(pos,
          s"Both branches of if should evaluate to the same type. " +
          s"Got: $tType and $eType instead.")

      if (condTyped.tp.isFunction)
        fail(pos, s"If condition should be of number type, got a function.")

      IfT(condTyped, thenTyped, elseTyped, tType)
  }

  private def checkLet(letExpr: Let, scope: TypeScope)
                      (implicit pos: Position): TypedTree =
    letExpr match {
      case Let(ident, value, body) =>
        val valueTyped = getType(value, scope)
        val identTyped = IdentT(ident.name, valueTyped.tp)
        val bodyTyped  = getType(body, updateScope(ident.name, valueTyped, scope))

        LetT(identTyped, valueTyped, bodyTyped, bodyTyped.tp)
    }

  private def updateScope(name: String, value: TypedTree, scope: TypeScope)
                         (implicit pos: Position): TypeScope = {
    val valType = value.tp
    scope + (name -> valType)
  }

  private def checkCall(call: Call, scope: TypeScope)
                       (implicit pos: Position): TypedTree = call match {
    case Call(fun, args) =>
      val argsTyped = args.map (getType (_, scope) )
      val funTyped  = getType(fun, scope)
      val arity     = args.length

      if (!funTyped.tp.isFunction)
        fail(pos, s"Trying to call a non-function object.")

      if (argsTyped.exists(_.tp.isFunction))
        fail(pos, s"Argument of non-number type.")

      if (arity == args.length)
        CallT(funTyped, argsTyped, DoubleType)
      else
        fail(pos, s"Function of $arity arguments called" +
            s"with ${args.length} arguments.")
  }

  private def checkClosure(cl: Closure, scope: TypeScope)
                          (implicit pos: Position): TypedTree = cl match {
    case Closure(params, body) =>
      val paramTypes  = (1 to params.length).map(_ => ParamType)
      val paramNames  = params.map(p => p.name)
      val paramsMap   = paramNames.zip(paramTypes).toMap
      val paramsTyped = params.map(p => IdentT(p.name, ParamType))

      ClosureT(paramsTyped, getType(body, scope ++ paramsMap),
        FunctionType(params.length))
  }

  private def formatError(pos: Position, msg: String): String =
    s"Error at line ${pos.line}, column ${pos.column}: " + msg

  private def fail(pos: Position, msg: String) =
    throw new TypecheckerException(formatError(pos, msg))
}
