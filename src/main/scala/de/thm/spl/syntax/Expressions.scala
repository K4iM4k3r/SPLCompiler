package de.thm.spl.syntax
import de.thm.spl.symbols.{ParameterSymbol, Scope, VariableSymbol}
import de.thm.spl.types
import de.thm.spl.types.ArrayType

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

  case class VariableReference(name: String) extends ReferenceExpression{
    override def getType(scope: Scope): types.Type = scope.lookup(name).get match {
      case ParameterSymbol(_, typ, _, _) => typ
      case VariableSymbol(_, typ, _) => typ
    }
  }
  case class ArrayAccess(reference: ReferenceExpression, index: ValueExpression) extends ReferenceExpression{
    override def getType(scope: Scope): types.Type = reference.getType(scope) match{
      case ArrayType(base, _, _) => base
    }
  }
}
