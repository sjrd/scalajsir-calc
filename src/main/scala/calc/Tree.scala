package calc

import org.scalajs.core.ir.Position

sealed trait TreeType
case object DoubleType extends TreeType
case class FunctionType(arity: Int) extends TreeType
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
