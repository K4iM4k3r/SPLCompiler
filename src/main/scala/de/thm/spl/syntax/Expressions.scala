package de.thm.spl.syntax

object Expressions {

  case class Add(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class Sub(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class Mul(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class Div(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class UnaryMinus(right: ValueExpression) extends ValueExpression
  case class IntegerLiteral(integer: Long) extends ValueExpression

  case class Less(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class LessEquals(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class Greater(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class GreaterEquals(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class Equals(left: ValueExpression, right: ValueExpression) extends ValueExpression
  case class NotEquals(left: ValueExpression, right: ValueExpression) extends ValueExpression

  case class Dereference(reference: ReferenceExpression) extends ValueExpression

  case class VariableReference(name: String) extends ReferenceExpression
  case class ArrayAccess(reference: ReferenceExpression, index: ValueExpression) extends ReferenceExpression
}
