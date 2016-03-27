package calc

/**
  * Types used to annotate the AST.
  */
sealed trait TreeType {
  def isFunction: Boolean = false
  def isParam: Boolean = false
  def typeSuff: String
}

case object DoubleType extends TreeType {
  override def typeSuff = "D"
}

// in our language every parameter is a Double, so we only
// need to know function's arity
case class FunctionType(arity: Int) extends TreeType {
  override def isFunction: Boolean = true
  override def typeSuff = "F"
}

/** We introduce a synthetic distinction between numbers and numbers-as-parameters
 * the reason is that IR's lambdas only handle parameters of AnyType, so we need
 * a type that behaves like a double during typechecking, but translates to AnyType. */
case object ParamType extends TreeType {
  override def isParam: Boolean = true
  override def typeSuff = "D"
}

case object NoType extends TreeType {
  override def typeSuff = "\u0000"
}