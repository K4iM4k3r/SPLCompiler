package de.thm.spl.Parser

object Tokens {
  sealed trait Token {
    def lexem: String
  }
  case class ErrorToken(override val lexem: String) extends Token
  case class Keyword(override val lexem: String) extends Token
  //TODO: Add remaining token classes
}
