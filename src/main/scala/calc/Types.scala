package calc

sealed abstract class Type

final case object NumberType extends Type

final case class FunctionType(arity: Integer) extends Type
