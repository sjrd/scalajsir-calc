package calc

import org.scalajs.core.ir.{Position, Types => irtpe}

sealed trait CompileError extends Throwable
case class Unexpected(pos: Position, unexpected: Tree, expected: String)
  extends CompileError {
  override def toString(): String = {
    s"""
       | At line ${pos.line}, column ${pos.column}
       | Unexpected $unexpected, expected $expected"
    """.stripMargin
  }
}
case class TypeError(pos: Position, expected: irtpe.Type, got: irtpe.Type)
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

