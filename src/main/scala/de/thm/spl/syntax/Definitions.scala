package de.thm.spl.syntax

object Definitions {
  case class TypeDefinition(name: String, typeExpression: TypeExpression) extends TopLevelDefinition
  case class ProcedureDefinition(name: String, parameters: List[ParameterDefinition], variables: List[VariableDefinition], body: List[Statement]) extends TopLevelDefinition

  case class ParameterDefinition(name: String, isRef: Boolean, typExp: TypeExpression) extends Definition
  case class VariableDefinition(name: String, typeExp: TypeExpression) extends Definition
}
