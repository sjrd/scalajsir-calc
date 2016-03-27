package calc

import org.scalajs.core.ir.Position


sealed abstract class Tree {
  def pos: Position
}

final case class Literal(value: Double)
                        (implicit val pos: Position) extends Tree

final case class Ident(name: String)
                      (implicit val pos: Position) extends Tree

final case class BinaryOp(op: String, lhs: Tree, rhs: Tree)(
    implicit val pos: Position) extends Tree

final case class Let(name: Ident, value: Tree, body: Tree)(
    implicit val pos: Position) extends Tree

final case class Closure(params: List[Ident], body: Tree)(
    implicit val pos: Position) extends Tree

final case class Call(fun: Tree, args: List[Tree])(
    implicit val pos: Position) extends Tree

final case class If(cond: Tree, thenp: Tree, elsep: Tree)(
    implicit val pos: Position) extends Tree
