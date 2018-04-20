package de.thm.spl.Parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.Reader

object SPLScanner extends Scanners with RegexParsers {

  override type Elem = Char
  override type Token = Tokens.Token

  override def errorToken(msg: String): Tokens.Token = Tokens.ErrorToken(msg)
  override def whitespace: Parser[Any] = """\s*""".r

  override def token: Parser[Tokens.Token] = {
    """array|else|if|of|proc|ref|type|var|while""".r    ^^ Tokens.Keyword     |
    """(0x(?:[a-f]|[A-F]|[0-9])+)""".r                  ^^ {x => Tokens.LiteralToken(x, Integer.parseInt(x.substring(2), 16))} |
    """(\'(?:.|\\n)\')""".r                             ^^ {case "\n" => Tokens.LiteralToken("\n", 10)
                                                            case a    => Tokens.LiteralToken(a, Char.char2int(a.charAt(1)))} |
    """(0|(?:[1-9][0-9]*))""".r                         ^^ {x => Tokens.LiteralToken(x, Integer.parseInt(x))} |
    """(?:\w|\_)+""".r                                  ^^ Tokens.IdentToken |
    """(\+|\-)""".r                                     ^^ Tokens.AddOpToken |
    """(\*|/|%)""".r                                    ^^ Tokens.MultOpToken |
    """(<=|>=|=|<|>|#)""".r                             ^^ Tokens.CompOpToken |
    """(:=)""".r                                        ^^ Tokens.AssignToken |
    """(:|,)""".r                                       ^^ Tokens.DefinitionToken |
    """(\(|\[|\{)""".r                                  ^^ Tokens.LeftBracketToken |
    """(\)|\]|\})""".r                                  ^^ Tokens.RightBracketToken |
    """(;)""".r                                         ^^ Tokens.SemicolonToken
  }

  def apply(code: String): Reader[Tokens.Token] = {
    new Scanner(code)
  }
}
