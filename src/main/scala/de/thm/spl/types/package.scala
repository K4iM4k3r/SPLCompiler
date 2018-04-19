package de.thm.spl

package object types {
  sealed trait Type
  case object ErrorType extends Type
  case object IntegerType extends Type
  case object BoolType extends Type
  case class ArrayType(base: Type, size: Long) extends Type
}
