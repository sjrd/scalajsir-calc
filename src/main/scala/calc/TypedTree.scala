package calc

import org.scalajs.core.ir.Position


sealed abstract class TypedTree {
  def pos: Position
  def tp: TreeType
}

final case class LiteralT(value: Double, tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class IdentT(name: String, tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class BinaryOpT(op: String, lhs: TypedTree, rhs: TypedTree, tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class LetT(name: IdentT, value: TypedTree, body: TypedTree, tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class ClosureT(params: List[IdentT], body: TypedTree, tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class CallT(fun: TypedTree, args: List[TypedTree], tp: TreeType)(
  implicit val pos: Position) extends TypedTree

final case class IfT(cond: TypedTree, thenp: TypedTree, elsep: TypedTree, tp: TreeType)(
  implicit val pos: Position) extends TypedTree
