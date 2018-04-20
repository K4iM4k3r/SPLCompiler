package de.thm.spl.Parser

import de.thm.PositionedMessageLogger
import de.thm.spl.Parser.Tokens._
import de.thm.spl.syntax.Definitions._
import de.thm.spl.syntax.Expressions._
import de.thm.spl.syntax.Statements._
import de.thm.spl.syntax.TypeExpressions.{ArrayTypeExpression, NamedTypeExpression}
import de.thm.spl.syntax._

import scala.util.parsing.combinator.Parsers

object SPLParser extends Parsers {
  override type Elem = Tokens.Token

  // parser for identifiers
  private val id_any : Parser[Any] =
    elem("identifier", _.isInstanceOf[IdentToken])

  private def ident: Parser[String] =
    id_any   ^^ { case (x: Any) => x.asInstanceOf[IdentToken].lexem }

  private val num_any : Parser[Any] =
    elem("number", _.isInstanceOf[LiteralToken])

  private def number: Parser[IntegerLiteral] =
    num_any ^^ { case (n : Any) => IntegerLiteral(n.asInstanceOf[LiteralToken].value)}

  private def arithExp: Parser[ValueExpression] = chainl1(term, term, addOp())

  private def addOp() : Parser[(ValueExpression, ValueExpression) ⇒ ValueExpression] =
      AddOpToken("+") ^^^ {(x:ValueExpression, y: ValueExpression) => Add(x,y)} |
      AddOpToken("-") ^^^ {(x:ValueExpression, y: ValueExpression) => Sub(x,y)}

  private def term: Parser[ValueExpression]  = chainl1(factor, factor, multOp)

  private def multOp : Parser[(ValueExpression, ValueExpression) ⇒ ValueExpression] =
      MultOpToken("*") ^^^ {(x:ValueExpression, y: ValueExpression) => Mul(x,y)}  |
      MultOpToken("/") ^^^ {(x:ValueExpression, y: ValueExpression) => Div(x,y)}

  private def factor: Parser[ValueExpression] =
      number |
      AddOpToken("-") ~> number ^^ {n => UnaryMinus(n)}
      LeftBracketToken("(") ~> arithExp <~ RightBracketToken(")") |
      refExp

  private def refExp: Parser[Dereference] =
    lExp ^^ {le => Dereference(le) }

  private def lExp: Parser[ReferenceExpression] =
      ident ~ (LeftBracketToken("[") ~> arithExp <~ RightBracketToken("]")) ^^ { case name ~ exp => ArrayAccess(VariableReference(name), exp) } |
      ident ^^ VariableReference

  private def boolExp: Parser[ValueExpression] =
    (arithExp <~ CompOpToken("<"))  ~ arithExp ^^ { case e1 ~ e2 => Less(e1, e2) } |
      (arithExp <~ CompOpToken(">"))  ~ arithExp ^^ { case e1 ~ e2 => Greater(e1, e2) } |
      (arithExp <~ CompOpToken("="))  ~ arithExp ^^ { case e1 ~ e2 => Equals(e1, e2) } |
      (arithExp <~ CompOpToken("!=")) ~ arithExp ^^ { case e1 ~ e2 => NotEquals(e1, e2) } |
      (arithExp <~ CompOpToken("<=")) ~ arithExp ^^ { case e1 ~ e2 => LessEquals(e1, e2) } |
      (arithExp <~ CompOpToken(">=")) ~ arithExp ^^ { case e1 ~ e2 => GreaterEquals(e1, e2) }

  private def typeExpr: Parser[TypeExpression] =
    arrayExpr | namedExpr

  private def arrayExpr: Parser[TypeExpression] =
    ((Keyword("array") ~ LeftBracketToken("[")) ~> number ~ ((RightBracketToken("]") ~ Keyword("of")) ~> (arrayExpr | namedExpr))) ^^ {case n ~ exp => ArrayTypeExpression(exp, n)}

  private def namedExpr: Parser[TypeExpression] =
    ident ^^ { n => NamedTypeExpression(n)}


  private def program: Parser[Program] = rep(topLevelDef) ^^ Program

  private def topLevelDef: Parser[TopLevelDefinition] =
    typeDef | procDef

  private def typeDef: Parser[TopLevelDefinition] =
    (Keyword("type") ~> ident ~ (CompOpToken("=") ~> typeExpr) <~ SemicolonToken(";")) ^^ { case name ~ expr => TypeDefinition(name, expr)}

  private def procDef: Parser[TopLevelDefinition] = {
    (Keyword("proc") ~> ident ~ (LeftBracketToken("(") ~> repsep(param, DefinitionToken(",")) <~ RightBracketToken(")")) ~ procBody) ^^ {
            case name ~ params ~ body => body match {
              case (vars, stmts) => ProcedureDefinition(name, params, vars, stmts)
              }
            }
  }

  private def procBody: Parser[(List[VariableDefinition], List[Statement])] =
    (LeftBracketToken("{") ~>  rep(vari) ~ rep(stmt) <~ RightBracketToken("}")) ^^ {case vars ~ stmts => (vars, stmts)}


  private def param: Parser[ParameterDefinition] =
    (Keyword("ref") ~> ident ~ (DefinitionToken(":") ~> typeExpr)) ^^ { case name ~ typ => ParameterDefinition(name, isRef = true, typ)} |
      (ident ~ (DefinitionToken(":") ~> typeExpr)) ^^ { case name ~ typ => ParameterDefinition(name, isRef = false, typ)}


  private def vari: Parser[VariableDefinition] =
    Keyword("var") ~> ident ~ (DefinitionToken(":") ~> typeExpr <~ SemicolonToken(";")) ^^ { case name ~ typ => VariableDefinition(name, typ)}


  private def stmt: Parser[Statement] =
      SemicolonToken(";") ^^ {x => EmptyStatement} |
      (lExp ~ (AssignToken(":=") ~> arithExp) <~ SemicolonToken(";")) ^^ {case ref ~ vale => Assignment(ref, vale)} |
      (Keyword("if") ~ LeftBracketToken("(") ~> boolExp ~ (RightBracketToken(")") ~> stmt) ~ (Keyword("else") ~> stmt)) ^^ {case bool ~ stmt1 ~ stmt2 => If(bool, stmt1, stmt2)} |
      (Keyword("if") ~ LeftBracketToken("(") ~> boolExp ~ (RightBracketToken(")") ~> stmt)) ^^ {case bool ~ stmt => If(bool, stmt, EmptyStatement)} |
      (Keyword("while") ~> LeftBracketToken("(") ~> boolExp ~ (RightBracketToken(")") ~> stmt)) ^^ {case bool ~ stmt => While(bool, stmt)} |
      (LeftBracketToken("{") ~> rep(stmt) <~ RightBracketToken("}")) ^^ CompoundStatement |
      (ident ~ (LeftBracketToken("(") ~> repsep(arithExp, DefinitionToken(",")) <~ (RightBracketToken(")") ~ SemicolonToken(";")))) ^^ {case name~args => Call(name, args)}


  def apply(code: String): Option[Program] = {
    val result = phrase(program)(SPLScanner(code))
    println(result)
    result match {
      case Success(t, _) => Some(t)
      case NoSuccess(msg, a) =>
        PositionedMessageLogger.logError(a.pos, msg)
        None
    }
  }
}
