package de.thm.spl.syntax

object Statements {
  case object EmptyStatement extends Statement
  case class Assignment(target: ReferenceExpression, value: ValueExpression) extends Statement
  case class If(condition: ValueExpression, thenStatements: Statement, elseStatements: Statement) extends Statement
  case class While(condition: ValueExpression, body: Statement) extends Statement
  case class CompoundStatement(body: List[Statement]) extends Statement
  case class Call(name: String, arguments: List[ValueExpression]) extends Statement
}
