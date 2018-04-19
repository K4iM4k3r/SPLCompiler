package de.thm.spl.Parser

import scala.util.matching.Regex
import scala.util.parsing.input.{Position, Reader}


/*
 * Lexical analysis
 */
class ProgScanner extends ProgToken {

  private val literalPatS  = """(0|(?:[1-9][0-9]*))"""
  private val litHexPatS  = """(0x(?:[a-f]|[A-F]|[0-9])+)"""
  private val litAsciiPatS = """(\`(?:.|\\n)\`)"""

  private val idPatS      = """((?:\w|\_)+)"""
  private val addOpPatS   = """(\+|\-)"""
  private val multOpPatS  = """(\*|/|%)"""
  private val compOpPatS  = """(<=|>=|=|<|>|#)"""
  private val assignPatS  = """(:=)"""
  private val keywordPatS = """(array|else|if|of|proc|ref|type|var|while)"""

  private val typPatS     = """(int)"""
  private val declarPatS     = """(:|,)"""

  private val leftBPatS      = """(\(|\[|\{)"""
  private val rightBPatS     = """(\)|\]|\})"""
  private val semicolonPatS  = """(;)"""


  private val LiteralPat    = literalPatS.r
  private val LitHexPat     = litHexPatS.r
  private val LitAsciiPat   = litAsciiPatS.r

  private val IdPat        = idPatS.r
  private val KeywordPat   = keywordPatS.r
  private val AddOpPat     = addOpPatS.r
  private val MultOpPat    = multOpPatS.r
  private val CompOpPat    = compOpPatS.r

  private val TypPat       = typPatS.r
  private val DefinitionPat    = declarPatS.r

  private val LeftPPat     = leftBPatS.r
  private val RightPPat    = rightBPatS.r
  private val AssignPat    = assignPatS.r
  private val SemicolonPat = semicolonPatS.r

  private val pats = List( KeywordPat, LitHexPat, LiteralPat, LitAsciiPat, AddOpPat, MultOpPat, CompOpPat, LeftPPat, RightPPat, AssignPat, SemicolonPat,TypPat, DefinitionPat, IdPat )

  private val whitespacePatS  = """\s+"""
  private val whitespacePat = whitespacePatS.r

  /**
    * used in parser to remove trailing whitespace
    * @return The pattern of whitespaces
    */
  def whitespacePattern: String = whitespacePatS


  private class ExpPosition(s: String, offset: Int) extends Position {
    var lineNr = 0
    var colNr  = 0
    var pos = 0
    var lineStart = 0
    while (pos < offset) {
      if (s.charAt(pos) == '\n') {
        lineStart = pos
        lineNr = lineNr + 1
        colNr = 0
      } else {
        colNr = colNr + 1
      }
      pos = pos + 1
    }
    override def line: Int = lineNr
    override def column: Int = colNr
    override def lineContents: String = s.substring(lineStart, pos)
  }

  /**
    * A "functional" scanner defined as Token reader.
    * Defined according to the example of scala.util.parsing.combinator.lexical.Scanners.Scanner
    * @param input    the string that is to be tokenized. Always passed unmodified to other scanners.
    * @param actPos   the start position within input, stat scanning at this position
    */
  class Scanner(input: String, private val actPos: Int = 0) extends Reader[Token] {

    // the position at which we look for a token
    private var actOffset = actPos

    // moves actOffset over whitespaces
    private def skipWhiteSpace(): Int =
      whitespacePat.findPrefixMatchOf(input.subSequence(actOffset, input.length())) match {
        case Some(m) => actOffset + m.end
        case None => actOffset
      }

    // Try to match r at actOffset with the input
    private def matchRegex(r: Regex): Option[String] = {
      r.findPrefixMatchOf(input.subSequence(actOffset, input.length())) match {
        case Some(matchedMatch) =>
          val res = Some(input.subSequence(actOffset, actOffset + matchedMatch.end).toString)
          actOffset = actOffset + matchedMatch.end
          res
        case None => None
      }
    }

    // skip whitespaces at the front
    actOffset = skipWhiteSpace()


    // look for a token
    private var matched: Option[String] = None
    private val startOffset = actOffset // fix actual position
    private var tokenIndex = 0

    while (matched.isEmpty && tokenIndex < pats.length) {
      matched = matchRegex(pats(tokenIndex))
      if (matched.isEmpty) { actOffset = startOffset }
      tokenIndex = tokenIndex+1
    }

    // fix token that was found and its position (if some token was found), and the position at its end
    val (tok: Token, tok_end: Int, tokPos: Position) = matched match {
      case None =>
        if (actOffset >= input.length)
          (EOF, input.length, new ExpPosition(input, actOffset))
        else {
          println(input.substring(actOffset))
          (errorToken("unexpected end of input"), input.length)
        }
      case Some(matchedStr) =>
        val pos = new ExpPosition(input, actOffset)
        matchedStr match {
          case LiteralPat(lexem)    => (LiteralToken(lexem, Integer.parseInt(lexem)), actOffset, pos)
          case LitHexPat(lexem)     => (LiteralToken(lexem, Integer.parseInt (lexem.substring(2),16)), actOffset, pos)
          case LitAsciiPat(lexem)   =>
            if(lexem.equals("""`\n`""")){
              (LiteralToken(lexem, 10), actOffset, pos)
            }
            else {
              (LiteralToken(lexem, Char.char2int(lexem.charAt(1))), actOffset, pos)
            }
          case AddOpPat(op)         => (AddOpToken(op), actOffset, pos)
          case MultOpPat(op)        => (MultOpToken(op), actOffset, pos)
          case CompOpPat(p)         => (CompOpToken(p), actOffset, pos)
          case LeftPPat(p)          => (LeftBracketToken(p), actOffset, pos)
          case RightPPat(p)         => (RightBracketToken(p), actOffset, pos)
          case KeywordPat(p)        => (KwToken(p), actOffset, pos)
          case TypPat(p)            => (TypToken(p), actOffset, pos)
          case DefinitionPat(p)     => (DefinitionToken(p), actOffset, pos)
          case IdPat(p)             => (IdentToken(p), actOffset, pos)
          case SemicolonPat(p)      => (SemicolonToken(p), actOffset, pos)
          case AssignPat(p)         => (AssignToken(p), actOffset, pos)
          case x                    => (ErrorToken("unexpected "+x), actOffset, pos)
        }
    }

    //watch for trailing whitespaces in the source
    //actOffset = skipWhiteSpace()


    // methods defined by reader
    override def pos: Position = tokPos

    override def first: Token = tok

    override def rest: Reader[Token] = new Scanner(input, actOffset)

    override def atEnd: Boolean = {
      actPos >= input.length
    }
  }

}
