package de.thm.spl.Parser

import scala.util.parsing.combinator.token.Tokens

class ProgToken extends Tokens {

  case class  LiteralToken(chars: String, value: Int)       extends Token
  case class  LeftBracketToken(chars: String)               extends Token
  case class  RightBracketToken(chars: String)              extends Token
  case class  AssignToken(chars: String)                    extends Token
  case class  SemicolonToken(chars: String)                 extends Token
  case class  AddOpToken(chars: String)                     extends Token
  case class  MultOpToken(chars: String)                    extends Token
  case class  CompOpToken(chars: String)                    extends Token
  case class  IdentToken(chars: String)                     extends Token
  case class  KwToken(chars: String)                        extends Token
  case class  TypToken(chars: String)                       extends Token
  case class  DefinitionToken(chars: String)                extends Token

}
