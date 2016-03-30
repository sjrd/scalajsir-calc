package calc

import org.scalajs.core.ir.{Position, Trees}

sealed trait CompileError extends Throwable
case class Unexpected(pos: Position, unexpected: Trees.Tree, expected: String)
  extends CompileError {
  override def toString(): String = {
    s"""
       | At line ${pos.line}, column ${pos.column}
       | Unexpected $unexpected, expected $expected"
    """.stripMargin
  }
}
case class TypeError(pos: Position, expected: Type, got: Type)
  extends CompileError {
  override def toString(): String = {
    s"""
       | At line ${pos.line}, column ${pos.column}
       | Expected type $expected, got $got
      """.stripMargin
  }
}
case class UnknownOperator(pos: Position, op: String) extends CompileError {
  override def toString(): String = {
    s"""
       | At line ${pos.line}, column ${pos.column}
       | Unknown operator '$op'
      """.stripMargin
  }
}
case class UnboundVariable(ident: Ident) extends CompileError {
  override def toString(): String = {
    s"""
       | Unbound variable ${ident.name} at line ${ident.pos.line}, column ${ident.pos.column}
     """.stripMargin
  }
}
case class InvalidNumberOfArgument(pos: Position, expected: Int, got: Int) extends CompileError {
  override def toString(): String = {
    s"""
       | Invalid number of argument at line ${pos.line}, column ${pos.column}
       | Expected $expected arguments, got $got
     """.stripMargin
  }
}

