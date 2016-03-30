package calc

object Typer {

  def inferType(tree: Tree): TreeT = {
    tree match {
      case t:Literal => implicit val pos = t.pos
        LiteralT(t.value)

      case t:BinaryOp => binaryOp(t)
    }
  }

  def binaryOp(t: BinaryOp) = { implicit val pos = t.pos
    val lhs = inferType(t.lhs)
    val rhs = inferType(t.rhs)
    if (lhs.getType() == rhs.getType()) {
      new BinaryOpT(t.op, lhs, rhs) {
        def getType() = lhs.getType()
      }
    } else {
      throw new TypeError(rhs.pos, lhs.getType(), rhs.getType())
    }
  }
}
