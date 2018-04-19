package de.thm.spl

import scala.util.parsing.input.Positional

package object syntax {
  case class Program(definitions: List[Definition])

  trait SyntaxElement extends Positional

  trait ReferenceExpression extends SyntaxElement

  trait ValueExpression extends SyntaxElement

  trait Statement extends SyntaxElement

  trait TypeExpression extends SyntaxElement

  trait Definition extends SyntaxElement
  trait TopLevelDefinition extends Definition
}
