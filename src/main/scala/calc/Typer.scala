package calc

object Typer {

  type TypeEnv = Map[String, Type]

  def emptyEnv = Map[String, Type]()

  def inferType(tree: Tree)(implicit env: TypeEnv): (TreeT, List[(String, Type)]) = {
    implicit val pos = tree.pos
    tree match {
      case t:Ident => ident(t)
      case t:Literal => (LiteralT(t.value), Nil)
      case t:BinaryOp => binaryOp(t)
      case t:Let => letBinding(t)
      case t:If => ifElse(t)
      case t:Closure => closure(t)
      case t:Call => call(t)
    }
  }

  def ident(t: Ident)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val result = env.getOrElse(t.name, throw new UnboundVariable(t))
    (new IdentT(t.name) { val tpe = result }, Nil)
  }

  def binaryOp(t: BinaryOp)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val (lhs, subs) = inferType(t.lhs)
    val (rhs, subs2) = inferType(t.rhs)(env ++ subs)
    (lhs.tpe, rhs.tpe) match {
      case (TAny(l), TAny(r)) =>
        val assumption = List(l -> TDouble, r -> TDouble)
        val (lhs2, subs3) = inferType(t.lhs)(env ++ subs ++ subs2 ++ assumption)
        val (rhs2, subs4) = inferType(t.rhs)(env ++ subs ++ subs2 ++ subs3 ++ assumption)
        (new BinaryOpT(t.op, lhs2, rhs2) { val tpe = TDouble }, subs ++ subs2 ++ subs3 ++ subs4 ++ assumption)
      case (TAny(l), TDouble) =>
        val assumption = List(l -> TDouble)
        val (lhs2, subs3) = inferType(t.lhs)(env ++ subs ++ subs2 ++ assumption)
        (new BinaryOpT(t.op, lhs2, rhs) { val tpe = TDouble }, subs ++ subs2 ++ subs3 ++ assumption)
      case (TDouble, TAny(r)) =>
        val assumption = List(r -> TDouble)
        val (rhs2, subs3) = inferType(t.rhs)(env ++ subs ++ subs2 ++ assumption)
        (new BinaryOpT(t.op, lhs, rhs2) { val tpe = TDouble }, subs ++ subs2 ++ subs3 ++ assumption)
      case (TDouble, TDouble) =>
        (new BinaryOpT(t.op, lhs, rhs) { val tpe = TDouble }, subs ++ subs2)
      case _ =>
        throw new TypeError(rhs.pos, lhs.tpe, rhs.tpe)
    }
  }

  def letBinding(t: Let)(implicit env: TypeEnv) = { implicit val pos = t.pos
    // In case of recursive function, use wildcard first, instantiate later if possible.
    // e.g if we found f(1) -> instantiate to TFun(1) and resume inference
    val (value, subs) = inferType(t.value)(env = env + (t.name.name -> TAny(t.name.name)))
    val (body, subs2) = inferType(t.body)(env = env + (t.name.name -> value.tpe) ++ subs)
    (new LetT(t.name, value, body) { val tpe = body.tpe }, subs2)
  }

  def ifElse(t: If)(implicit env: TypeEnv) = { implicit val pos = t.pos
    val (cond, subs) = inferType(t.cond)
    if (cond.tpe != TDouble) {
      throw new TypeError(cond.pos, TDouble, cond.tpe)
    } else {
      val (thenp, subs2) = inferType(t.thenp)(env ++ subs)
      val (elsep, subs3) = inferType(t.elsep)(env ++ subs ++ subs2)
      if (thenp.tpe != elsep.tpe) {
        throw new TypeError(elsep.pos, thenp.tpe, elsep.tpe)
      } else {
        (new IfT(cond, thenp, elsep) { val tpe = thenp.tpe }, subs3)
      }
    }
  }

  def closure(t: Closure)(implicit env: TypeEnv) = { implicit val pos = t.pos
    // Ensure all params are unique
    val paramSet = t.params.map({ _.name }).toSet
    paramSet foreach { p =>
      if (t.params.count({ _.name == p }) > 1) {
        throw new ParameterRedeclaration(pos, p)
      }
    }

    val params = t.params map { p => new IdentT(p.name) { val tpe = TDouble } }
    val (body, subs) = inferType(t.body)(env ++ (t.params map { _.name -> TDouble }))
    if (body.tpe == TDouble) {
      (new ClosureT(params, body) { val tpe = TFun(params.length) }, subs)
    } else {
      throw new TypeError(body.pos, TDouble, body.tpe)
    }
  }

  def call(t: Call)(implicit env: TypeEnv): (TreeT, List[(String, Type)]) = { implicit val pos = t.pos
    val (funType, subs) = inferType(t.fun)

    // All arguments must be double
    val init: (List[TreeT], List[(String, Type)]) = (Nil, subs)
    val (typedArgs, subs2) = t.args.foldRight(init) { (t, acc) =>
      val (lst, subs) = acc
      val (typed, subs2) = inferType(t)(env ++ subs)
      if (typed.tpe == TDouble) {
        (typed :: lst, subs ++ subs2)
      } else {
        throw new TypeError(t.pos, TDouble, typed.tpe)
      }
    }

    funType.tpe match {
      case TAny(name) =>
        val (tree, subs) = call(t)(env + (name -> TFun(t.args.size)))
        (tree, subs ++ List((name, TFun(t.args.size))))
      case TFun(arity)  =>
        val calleeArity = t.args.length
        // Quick exit if parameter length does not match
        if (arity != calleeArity) {
          throw new InvalidNumberOfArgument(pos, arity, calleeArity)
        } else {
          (new CallT(funType, typedArgs) { val tpe = TDouble }, subs2)
        }
      case TStaticForeignFun(clsName, methodName, arity) =>
        val calleeArity = t.args.length
        // Quick exit if parameter length does not match
        if (arity != calleeArity) {
          throw new InvalidNumberOfArgument(pos, arity, calleeArity)
        } else {
          (new ForeignCallT(clsName, methodName, typedArgs) { val tpe = TDouble }, subs2)
        }
      case other =>
        throw new TypeError(funType.pos, TFun(t.args.length), other)
    }
  }
}
