package de.thm.spl

package object symbols {
  sealed trait Symbol {
    def name: String
  }
  case class ParameterSymbol(override val name: String, typ: types.Type, isRef: Boolean) extends Symbol
  case class VariableSymbol(override val name: String, typ: types.Type) extends Symbol
  case class TypeSymbol(override val name: String, typ: types.Type) extends Symbol
  case class ProcedureSymbol(override val name: String, parameters: List[ParameterSymbol], variables: List[VariableSymbol]) extends Symbol
}
