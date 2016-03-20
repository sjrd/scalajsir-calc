package calc

import org.scalajs.core.ir.Position

sealed abstract class Tree {
  def pos: Position
}

final case class Literal(value: Double)(implicit val pos: Position) extends Tree

final case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(
    implicit val pos: Position) extends Tree
