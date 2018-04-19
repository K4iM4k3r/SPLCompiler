package de.thm.puck.assembler

import scala.util.parsing.input.Positional

object Tokens {
  sealed trait Token extends Positional
  case class Instruction(instruction: String) extends Token
  case class Directive(directive: String) extends Token
  case class Register(register: Int) extends Token
  case class Identifier(identifier: String) extends Token
  case class Delimiter(delimiter: String) extends Token
  case class Number(value: Long) extends Token
  case object NewLineToken extends Token
  case class CommentToken(comment: String) extends Token
}
