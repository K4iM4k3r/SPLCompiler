package de.thm.spl.Parser

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.Reader

object SPLScanner extends Scanners with RegexParsers {

  override type Elem = Char
  override type Token = Tokens.Token

  override def errorToken(msg: String): Tokens.Token = Tokens.ErrorToken(msg)
  override def whitespace: Parser[Any] = ??? //TODO: Insert regex for all whitespace

  override def token: Parser[Tokens.Token] = {
    """proc""".r ^^ Tokens.Keyword | //TODO: Extend regex with remaining keywords
      ??? //TODO: Add additional token classes with the | operator. See https://static.javadoc.io/org.scala-lang.modules/scala-parser-combinators_2.12/1.1.0/scala/util/parsing/combinator/Parsers.html for details
  }

  def apply(code: String): Reader[Tokens.Token] = {
    new Scanner(code)
  }
}
