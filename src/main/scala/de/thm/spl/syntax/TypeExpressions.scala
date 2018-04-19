package de.thm.spl.syntax

import de.thm.spl.syntax.Expressions.IntegerLiteral

object TypeExpressions {
  case class ArrayTypeExpression(base: TypeExpression, size: IntegerLiteral) extends TypeExpression
  case class NamedTypeExpression(symbol: String) extends TypeExpression
}
