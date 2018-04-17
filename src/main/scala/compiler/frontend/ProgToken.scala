package compiler.frontend

import scala.util.parsing.combinator.token.Tokens

class ProgToken extends Tokens {

  case class  NumberToken(chars: String)    extends Token
  case class  LeftPToken(chars: String)     extends Token
  case class  RightPToken(chars: String)    extends Token
  case class  AssignToken(chars: String)    extends Token
  case class  SemicolonToken(chars: String) extends Token
  case class  AddOpToken(chars: String)     extends Token
  case class  MultOpToken(chars: String)    extends Token
  case class  CompOpToken(chars: String)    extends Token
  case class  IdentToken(chars: String)     extends Token
  case class  KwToken(chars: String)        extends Token
  case class  TypToken(chars: String)       extends Token
  case class  DefinitionToken(chars: String) extends Token

}
