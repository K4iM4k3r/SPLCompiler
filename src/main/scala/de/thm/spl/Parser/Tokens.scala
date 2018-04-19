package de.thm.spl.Parser

object Tokens {
  sealed trait Token {
    def lexem: String
  }
  case class  ErrorToken(override val lexem: String) extends Token
  case class  Keyword(override val lexem: String) extends Token
  case class  LiteralToken(override val lexem: String, value: Int)       extends Token
  case class  LeftBracketToken(override val lexem: String)               extends Token
  case class  RightBracketToken(override val lexem: String)              extends Token
  case class  AssignToken(override val lexem: String)                    extends Token
  case class  SemicolonToken(override val lexem: String)                 extends Token
  case class  AddOpToken(override val lexem: String)                     extends Token
  case class  MultOpToken(override val lexem: String)                    extends Token
  case class  CompOpToken(override val lexem: String)                    extends Token
  case class  IdentToken(override val lexem: String)                     extends Token
  case class  KwToken(override val lexem: String)                        extends Token
  case class  TypToken(override val lexem: String)                       extends Token
  case class  DefinitionToken(override val lexem: String)                extends Token
}
