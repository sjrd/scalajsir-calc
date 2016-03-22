package calc

import org.scalajs.core.ir.{Types => irtpe, Position}

/**
 * A simple typechecker.
 * Checks whether the types match or are otherwise appropriate (e.g. in the if's condition).
 * Annotates AST with types, so that they can be used during compilation.
 * TODO group errors and fail as late as possible
 */

object Typechecker {

  class TypecheckerException(msg: String) extends Exception(msg)

  type TypeScope = Map[String, TreeType]

  def getScalaJSType(tp: TreeType): irtpe.Type = tp match {
    case DoubleType   => irtpe.DoubleType
    case FunctionType => irtpe.NoType     // TODO function type representation
    case NoType       => irtpe.NoType
  }

  def getType(tree: Tree, scope: TypeScope)(implicit pos: Position): Tree =  tree match {
    case Literal(x, _)                  => Literal(x, DoubleType)
    case b@BinaryOp(_, _, _, _)         => checkBinop(b, scope)
    case id@Ident(name, _)              => checkIdent(name, scope)
    case let@Let(ident, value, body, _) => checkLet(let, scope)
    case ifExpr@If(_, _, _, _)          => checkIf(ifExpr, scope)
    case Call(fun, args, _)             => ??? // TODO, should infer parameter types
    case Closure(params, body, _)       => ??? // TODO, parameter types
  }

  def typecheck(tree: Tree)(implicit pos: Position): Tree = {
    getType(tree, Map.empty[String, TreeType])
  }

  private def checkBinop(binop: BinaryOp, scope: TypeScope)(implicit pos: Position): Tree = {
    val lhsTyped = getType(binop.lhs, scope)
    val rhsTyped = getType(binop.rhs, scope)
    val lType = lhsTyped.tp
    val rType = rhsTyped.tp
    if (lType == rType && lType == DoubleType) BinaryOp(binop.op, lhsTyped, rhsTyped, lType)
    else fail(binop.pos, s"Incompatible operand types for ${binop.op}: $lType, $rType")
  }

  private def checkIdent(name: String, scope: TypeScope)(implicit pos: Position): Tree =
    scope.get(name) match {
      case Some(t) => Ident(name, t)
      case None => fail(pos, s"Not in scope: $name")
    }

  private def checkIf(ifExpr: If, scope: TypeScope)(implicit pos: Position): Tree = ifExpr match {
    case If(lit@Literal(_, _), thenp, elsep, _) =>
      val thenTyped = getType(thenp, scope)
      val elseTyped = getType(elsep, scope)
      val tType = thenTyped.tp
      val eType = elseTyped.tp

      if (tType != eType)
        fail(ifExpr.pos,
          s"Both branches of if should evaluate to the same type. " +
          s"Got: $tType and $eType instead.")

      If(getType(lit, scope), thenTyped, elseTyped, tType)

    case _ => fail(ifExpr.pos, s"If condition should be a number")
  }

  private def checkLet(letExpr: Let, scope: TypeScope)(implicit pos: Position): Tree =
    letExpr match {
      case Let(ident, value, body, _) =>
        val valueTyped = getType(value, scope)
        val bodyTyped = getType(body, updateScope(ident.name, valueTyped, scope))
        Let(ident, valueTyped, bodyTyped, bodyTyped.tp)
    }

  private def updateScope(name: String, value: Tree, scope: TypeScope)(implicit pos: Position): TypeScope = {
    val valType = getType(value, scope).tp
    scope + (name -> valType)
  }

  private def formatError(pos: Position, msg: String): String =
    s"Error at line ${pos.line}, column ${pos.column}: " + msg

  private def fail(pos: Position, msg: String) = throw new TypecheckerException(formatError(pos, msg))
}
