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
    case FunctionType(_) => irtpe.AnyType
  }

  def getType(tree: Tree, scope: TypeScope): TypedTree =  tree match {
    case l:      Literal  => LiteralT(l.value, DoubleType)(l.pos)
    case b:      BinaryOp => checkBinop(b, scope)
    case id:     Ident    => checkIdent(id, scope)
    case let:    Let      => checkLet(let, scope)
    case ifExpr: If       => checkIf(ifExpr, scope)
    case call:   Call     => checkCall(call, scope)
    case cl:     Closure  => checkClosure(cl, scope)
  }

  def typecheck(tree: Tree): TypedTree = {
    getType(tree, Map.empty[String, TreeType])
  }

  private def checkBinop(binop: BinaryOp, scope: TypeScope): TypedTree = {
    val lhsTyped = getType(binop.lhs, scope)
    val rhsTyped = getType(binop.rhs, scope)
    val lType    = lhsTyped.tp
    val rType    = rhsTyped.tp

    if (lType.isFunction || rType.isFunction)
      fail(binop.pos, s"Incompatible operand types for ${binop.op}: $lType, $rType")

    BinaryOpT(binop.op, lhsTyped, rhsTyped, lType)(binop.pos)
  }

  private def checkIdent(ident: Ident, scope: TypeScope): TypedTree =
    scope.get(ident.name) match {
      case Some(t) => IdentT(ident.name, t)(ident.pos)
      case None    =>
        if (stdlib.isLibFunction(ident.name))
          IdentT(ident.name, stdlib.libFunType(ident.name))(ident.pos)
        else
          fail(ident.pos, s"Not in scope: ${ident.name}")
    }

  private def checkIf(ifExpr: If, scope: TypeScope): TypedTree = ifExpr match {
    case If(cond, thenp, elsep) =>
      val condTyped = getType(cond, scope)
      val thenTyped = getType(thenp, scope)
      val elseTyped = getType(elsep, scope)
      val tType     = thenTyped.tp
      val eType     = elseTyped.tp

      if (tType != eType)
        fail(ifExpr.pos,
          s"Both branches of if should evaluate to the same type. " +
          s"Got: $tType and $eType instead.")

      if (condTyped.tp.isFunction)
        fail(ifExpr.pos, s"If condition should be of number type, got a function.")

      IfT(condTyped, thenTyped, elseTyped, tType)(ifExpr.pos)
  }

  private def checkLet(letExpr: Let, scope: TypeScope): TypedTree =
    letExpr match {
      case Let(ident, value, body) =>
        val valueTyped = getType(value, scope)
        val identTyped = IdentT(ident.name, valueTyped.tp)(ident.pos)
        val bodyTyped  = getType(body, updateScope(ident.name, valueTyped, scope))

        LetT(identTyped, valueTyped, bodyTyped, bodyTyped.tp)(letExpr.pos)
    }

  private def updateScope(name: String, value: TypedTree, scope: TypeScope): TypeScope = {
    val valType = value.tp
    scope + (name -> valType)
  }

  private def checkCall(call: Call, scope: TypeScope): TypedTree = call match {
    case Call(fun, args) =>
      val argsTyped = args.map (getType (_, scope) )
      val funTyped  = getType(fun, scope)
      val arity     = args.length

      if (!funTyped.tp.isFunction)
        fail(call.pos, s"Trying to call a non-function object.")

      if (argsTyped.exists(_.tp.isFunction))
        fail(call.pos, s"Argument of non-number type.")

      if (arity != args.length)
        fail(call.pos, s"Function of $arity arguments called" +
            s"with ${args.length} arguments.")

      CallT(funTyped, argsTyped, DoubleType)(call.pos)
  }

  private def checkClosure(cl: Closure, scope: TypeScope): TypedTree = cl match {
    case Closure(params, body) =>
      val paramTypes  = (1 to params.length).map(_ => DoubleType)
      val paramNames  = params.map(p => p.name)
      val paramsMap   = paramNames.zip(paramTypes).toMap
      val paramsTyped = params.map(p => IdentT(p.name, DoubleType)(p.pos))

      ClosureT(paramsTyped, getType(body, scope ++ paramsMap),
        FunctionType(params.length))(cl.pos)
  }

  private def formatError(pos: Position, msg: String): String =
    s"Error at line ${pos.line}, column ${pos.column}: " + msg

  private def fail(pos: Position, msg: String) =
    throw new TypecheckerException(formatError(pos, msg))
}
