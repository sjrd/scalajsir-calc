package calc

import org.scalajs.core.ir.{Types => irtpe }

object Typer {

  type TypeEnv = Map[String, Type]

  def emptyEnv = Map[String, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): TreeT = {
    implicit val pos = tree.pos
    tree match {
      case t:Ident =>
        val result = env.getOrElse(t.name, throw new UnboundVariable(t))
        new IdentT(t.name) { val tpe = result }
      case t:Literal => LiteralT(t.value)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
      case t:If => ifElse(t)
    }
  }

  def binaryOp(t: BinaryOp)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val lhs = inferType(t.lhs)
    val rhs = inferType(t.rhs)
    if (lhs.tpe == TDouble && lhs.tpe == rhs.tpe) {
      new BinaryOpT(t.op, lhs, rhs) { val tpe = lhs.tpe }
    } else {
      throw new TypeError(rhs.pos, lhs.tpe, rhs.tpe)
    }
  }

  def letBinding(t: Let)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val value = inferType(t.value)
    val body = inferType(t.body)(env = env + (t.name.name -> value.tpe))
    new LetT(t.name, value, body) { val tpe = body.tpe }
  }

  def ifElse(t: If)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val cond = inferType(t.cond)
    if (cond.tpe != TDouble) {
      throw new TypeError(cond.pos, TDouble, cond.tpe)
    } else {
      val thenp = inferType(t.thenp)
      val elsep = inferType(t.elsep)
      if (thenp.tpe != elsep.tpe) {
        throw new TypeError(elsep.pos, thenp.tpe, elsep.tpe)
      } else {
        new IfT(cond, thenp, elsep) {
          val tpe = thenp.tpe
        }
      }
    }
  }
}
