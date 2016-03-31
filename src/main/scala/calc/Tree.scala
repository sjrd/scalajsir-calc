package calc

import org.scalajs.core.ir.{Position, Types => irtpe}

sealed trait Type {
  def irtype: irtpe.Type
}

case object TAny extends Type {
  def irtype = irtpe.AnyType
}

case object TDouble extends Type {
  def irtype = irtpe.DoubleType
}

case class TFun(arity: Int) extends Type {
  def irtype = irtpe.AnyType
}

case class TStaticForeignFun(clsName: String, method: String, arity: Int) extends Type {
  def irtype = irtpe.AnyType
}

sealed abstract class Tree {
  def pos: Position
}

final case class Literal(value: Double)(implicit val pos: Position) extends Tree

final case class Ident(name: String)(implicit val pos: Position) extends Tree

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

sealed abstract class TreeT {
  def pos: Position
  val tpe: Type
}

final case class LiteralT(value: Double)(implicit val pos: Position) extends TreeT {
  val tpe = TDouble
}

abstract case class IdentT(name: String)(implicit val pos: Position) extends TreeT

abstract case class BinaryOpT(op: String, lhs: TreeT, rhs: TreeT)(
  implicit val pos: Position) extends TreeT

abstract case class LetT(name: Ident, value: TreeT, body: TreeT)(
  implicit val pos: Position) extends TreeT

abstract case class ClosureT(params: List[IdentT], body: TreeT)(
  implicit val pos: Position) extends TreeT

abstract case class CallT(fun: TreeT, args: List[TreeT])(
  implicit val pos: Position) extends TreeT

abstract case class ForeignCallT(clsName: String, methodName: String, args: List[TreeT])(
  implicit val pos: Position) extends TreeT

abstract case class IfT(cond: TreeT, thenp: TreeT, elsep: TreeT)(
  implicit val pos: Position) extends TreeT

