package calc

object Typer {

  type TypeEnv = Map[Ident, Type]

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    tree match {
      case t:Literal => implicit val pos = t.pos
        LiteralT(t.value)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
    }
  }

  def binaryOp(t: BinaryOp)(implicit env: TypeEnv) = { implicit val pos = t.pos
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

  def letBinding(t: Let)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val value = inferType(t.value)
    val body = inferType(t.body)(env = env + (t.name -> value.getType()))
    new LetT(t.name, value, body) {
      def getType = body.getType()
    }
  }
}
