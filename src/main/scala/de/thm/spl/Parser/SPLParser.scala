package de.thm.spl.Parser

import de.thm.PositionedMessageLogger
import de.thm.spl.syntax

import scala.util.parsing.combinator.Parsers

object SPLParser extends Parsers {
  override type Elem = Tokens.Token

  private def program: Parser[syntax.Program] = ???

  def apply(code: String): Option[syntax.Program] = {
    val result = phrase(program)(SPLScanner(code))

    result match {
      case Success(t, _) => Some(t)
      case NoSuccess(msg, a) =>
        PositionedMessageLogger.logError(a.pos, msg)
        None
    }
  }
}
