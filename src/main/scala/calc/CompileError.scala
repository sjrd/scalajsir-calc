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

