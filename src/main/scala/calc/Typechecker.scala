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
             (implicit pos: Position): Tree =  tree match {
    case Literal(x, _)    => Literal(x, DoubleType)
    case b:      BinaryOp => checkBinop(b, scope)
    case id:     Ident    => checkIdent(id, scope)
    case let:    Let      => checkLet(let, scope)
    case ifExpr: If       => checkIf(ifExpr, scope)
    case call:   Call     => checkCall(call, scope)
    case cl:     Closure  => checkClosure(cl, scope)
  }

  def typecheck(tree: Tree)(implicit pos: Position): Tree = {
    getType(tree, Map.empty[String, TreeType])
  }

  private def checkBinop(binop: BinaryOp, scope: TypeScope)
                        (implicit pos: Position): Tree = {
    val lhsTyped = getType(binop.lhs, scope)
    val rhsTyped = getType(binop.rhs, scope)
    val lType    = lhsTyped.tp
    val rType    = rhsTyped.tp

    if (!lType.isFunction && !rType.isFunction)
      BinaryOp(binop.op, lhsTyped, rhsTyped, lType)
    else
      fail(binop.pos, s"Incompatible operand types for ${binop.op}: $lType, $rType")
  }

  private def checkIdent(ident: Ident, scope: TypeScope)
                        (implicit pos: Position): Tree =
    scope.get(ident.name) match {
      case Some(t) => Ident(ident.name, t)
      case None    => fail(pos, s"Not in scope: ${ident.name}")
    }

  private def checkIf(ifExpr: If, scope: TypeScope)
                     (implicit pos: Position): Tree = ifExpr match {
    case If(cond, thenp, elsep, _) =>
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

      If(getType(condTyped, scope), thenTyped, elseTyped, tType)
  }

  private def checkLet(letExpr: Let, scope: TypeScope)
                      (implicit pos: Position): Tree =
    letExpr match {
      case Let(ident, value, body, _) =>
        val valueTyped = getType(value, scope)
        val identTyped = Ident(ident.name, valueTyped.tp)
        val bodyTyped  = getType(body, updateScope(ident.name, valueTyped, scope))

        Let(identTyped, valueTyped, bodyTyped, bodyTyped.tp)
    }

  private def updateScope(name: String, value: Tree, scope: TypeScope)
                         (implicit pos: Position): TypeScope = {
    val valType = getType(value, scope).tp
    scope + (name -> valType)
  }

  private def checkCall(call: Call, scope: TypeScope)
                       (implicit pos: Position): Tree = call match {
    case Call(fun, args, _) => fun match {
      case Ident(fname, _) =>
        val argsTyped       = args.map (getType (_, scope) )
        // check if function is in scope:
        val ftype: TreeType = scope.getOrElse(fname, fail(pos, s"Not in scope: $fname"))

        // check if all args are numbers:
        if (argsTyped.exists(_.tp.isFunction))
          fail(pos, s"Argument of non-number type.")

        // check if the arity is right:
        ftype match {
          case FunctionType(arity) if arity == args.length =>
            Call(getType(fun, scope), argsTyped, DoubleType)

          case FunctionType(arity) =>
            fail(pos, s"Function of $arity arguments called" +
                s"with ${args.length} arguments.")

          case _ => fail(pos, s"Trying to call a non-function object.")
        }

      case _ =>
        fail(pos, s"Trying to call a non-function object 2.")
      }
  }

  private def checkClosure(cl: Closure, scope: TypeScope)
                          (implicit pos: Position): Tree = cl match {
    case Closure(params, body, _) =>
      val paramTypes  = (1 to params.length).map(_ => ParamType)
      val paramNames  = params.map(p => p.name)
      val paramsMap   = paramNames.zip(paramTypes).toMap
      val paramsTyped = params.map(p => Ident(p.name, ParamType))

      Closure(paramsTyped, getType(body, scope ++ paramsMap),
        FunctionType(params.length))
  }

  private def formatError(pos: Position, msg: String): String =
    s"Error at line ${pos.line}, column ${pos.column}: " + msg

  private def fail(pos: Position, msg: String) =
    throw new TypecheckerException(formatError(pos, msg))
}
