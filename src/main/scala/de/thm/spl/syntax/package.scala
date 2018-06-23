package de.thm.spl

import de.thm.spl.symbols.Scope
import de.thm.spl.types.Type

import scala.util.parsing.input.Positional

package object syntax {
  case class Program(definitions: List[Definition])

  trait SyntaxElement extends Positional

  trait ReferenceExpression extends SyntaxElement{
    def getType(scope: Scope): Type
  }

  trait ValueExpression extends SyntaxElement

  trait Statement extends SyntaxElement

  trait TypeExpression extends SyntaxElement

  trait Definition extends SyntaxElement
  trait TopLevelDefinition extends Definition
}
