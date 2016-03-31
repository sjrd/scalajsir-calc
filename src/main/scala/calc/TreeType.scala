package calc

/**
  * Types used to annotate the AST.
  */
sealed trait TreeType {
  def isFunction: Boolean = false
  def isNumber: Boolean = true
  def typeSuff: String
}

case object DoubleType extends TreeType {
  override def typeSuff = "D"
}

// in our language every parameter is a Double, so we only
// need to know function's arity
case class FunctionType(arity: Int) extends TreeType {
  override def isFunction: Boolean = true
  override def isNumber: Boolean = false
  override def typeSuff = "F"
}
