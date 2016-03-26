package calc

import org.scalajs.core.ir.Position

sealed trait TreeType {
  def isFunction: Boolean = false
  def isParam: Boolean = false
}
case object DoubleType extends TreeType
case class FunctionType(arity: Int) extends TreeType {
  override def isFunction: Boolean = true
}
// we introduce a synthetic distinction between numbers and numbers-as-parameters
// the reason is that IR's lambdas only handle parameters of AnyType, so we need
// a type that behaves like a double during typechecking, but translates to AnyType
case object ParamType extends TreeType {
  override def isParam: Boolean = true
}
case object NoType extends TreeType

sealed abstract class Tree {
  def pos: Position
  def tp: TreeType
}

final case class Literal(value: Double, tp: TreeType=NoType)(implicit val pos: Position) extends Tree

final case class Ident(name: String, tp: TreeType=NoType)(implicit val pos: Position) extends Tree

final case class BinaryOp(op: String, lhs: Tree, rhs: Tree, tp: TreeType=NoType)(
    implicit val pos: Position) extends Tree

final case class Let(name: Ident, value: Tree, body: Tree, tp: TreeType=NoType)(
    implicit val pos: Position) extends Tree

final case class Closure(params: List[Ident], body: Tree, tp: TreeType=NoType)(
    implicit val pos: Position) extends Tree

final case class Call(fun: Tree, args: List[Tree], tp: TreeType=NoType)(
    implicit val pos: Position) extends Tree

final case class If(cond: Tree, thenp: Tree, elsep: Tree, tp: TreeType=NoType)(
    implicit val pos: Position) extends Tree
