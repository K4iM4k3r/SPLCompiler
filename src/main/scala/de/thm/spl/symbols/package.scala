package de.thm.spl

import de.thm.spl.types._

package object symbols {
  sealed trait Symbol {
    def name: String
  }
  case class ParameterSymbol(override val name: String, typ: Type, isRef: Boolean, offset: Long) extends Symbol
  case class VariableSymbol(override val name: String, typ: Type, offset: Long) extends Symbol
  case class TypeSymbol(override val name: String, typ: Type) extends Symbol
  case class ProcedureSymbol(override val name: String, parameters: List[ParameterSymbol], variables: List[VariableSymbol], offset: Long) extends Symbol
}
