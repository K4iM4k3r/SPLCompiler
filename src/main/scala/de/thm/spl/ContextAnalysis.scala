package de.thm.spl

import de.thm.MessageLogger
import de.thm.spl.symbols._
import de.thm.spl.syntax.Definitions._
import de.thm.spl.syntax.Expressions._
import de.thm.spl.syntax.Statements._
import de.thm.spl.syntax.TypeExpressions.{ArrayTypeExpression, NamedTypeExpression}
import de.thm.spl.syntax.{ReferenceExpression, Statement, TypeExpression, ValueExpression}
import de.thm.spl.types._

import scala.collection.mutable.ListBuffer

object ContextAnalysis {
  var symbolTable: SymbolTable = new SymbolTable

  def apply(abstractSyntax: syntax.Program): SymbolTable = {
    val storedProcedure: ListBuffer[ProcedureDefinition] = new ListBuffer[ProcedureDefinition]
    symbolTable = new SymbolTable
    // Default typ and library functions
    initSymbolTable()

    // Definition of Types and Procedures
    abstractSyntax.definitions.foreach {
      case TypeDefinition(name, typexp) =>
        symbolTable.define(TypeSymbol(name, conversionExpressionToTyp(typexp)))
      case proc@ProcedureDefinition(name, params, variables, _) =>
        storedProcedure += proc
        symbolTable.define(ProcedureSymbol(name, params.map(p => {
          ParameterSymbol(p.name, conversionExpressionToTyp(p.typExp), p.isRef)
        }), variables.map(v => {
          VariableSymbol(v.name, conversionExpressionToTyp(v.typeExp))
        })))
    }

    //checking of Variable, Parameter usage
    storedProcedure.foreach(p => {
      val scope: Scope = symbolTable.childScope(p.name).get
      p.body.foreach(s => checkStatement(scope, s))
    })

    symbolTable
  }

  def initSymbolTable(): Unit = {
    symbolTable.define(TypeSymbol("int", IntegerType))
    symbolTable.define(ProcedureSymbol("printi", List(ParameterSymbol("i", IntegerType, isRef = false)), List()))
    symbolTable.define(ProcedureSymbol("printc", List(ParameterSymbol("i", IntegerType, isRef = false)), List()))
    symbolTable.define(ProcedureSymbol("readi", List(ParameterSymbol("i", IntegerType, isRef = true)), List()))
    symbolTable.define(ProcedureSymbol("readc", List(ParameterSymbol("i", IntegerType, isRef = true)), List()))
    symbolTable.define(ProcedureSymbol("exit", List(), List()))
    symbolTable.define(ProcedureSymbol("time", List(ParameterSymbol("i", IntegerType, isRef = true)), List()))

  }

  def checkStatement(scope: Scope, statement: Statement): Unit = {
    statement match {
      case Assignment(target, value) =>
        (checkAndGetTyp(scope, target), checkValueExpression(scope, value)) match {
          case (IntegerType, IntegerType) => // okay
          case (IntegerType, _) => MessageLogger.logError(s"$value is not of the typ int")
          case (_, IntegerType) => MessageLogger.logError(s"$target is not of the typ int")
          case _ => MessageLogger.logError(s"($target) and ($value) are both not of the typ int")
        }
      case If(condition, thenStatements, elseStatements) =>
        checkValueExpression(scope, condition) match {
          case BoolType =>
            checkStatement(scope, thenStatements)
            checkStatement(scope, elseStatements)
          case _ => MessageLogger.logError(s"Illegal typ of $condition of if")
        }
      case While(condition, body) => checkValueExpression(scope, condition) match {
        case BoolType => checkStatement(scope, body)
        case _ => MessageLogger.logError(s"Illegal typ of $condition of while")
      }

      case CompoundStatement(body) =>
        body.foreach(bp => checkStatement(scope, bp))
      case Call(name, arguments) =>
        symbolTable.lookup(name) match {
          case Some(p@ProcedureSymbol(_, _, _)) =>
            if (p.parameters.length != arguments.length) {
              MessageLogger.logError("Number of Arguments are not equal with Procedure-Definition")
            }
            else {
              arguments.zip(p.parameters).foreach {
                case (Dereference(ref), param) =>
                  if (param.isRef) {
                    //                    MessageLogger.logWarning(s"$ref must not deref $param")//okay
                  }
                case (some, param) =>
                  if (param.isRef) {
                    MessageLogger.logError(s"$some is not ref")
                  }
                  else {
                    checkValueExpression(scope, some) match {
                      case IntegerType =>
                      case _ => MessageLogger.logError(s"$some must be an int")
                    }
                  }
              }
            }
          case Some(_) => MessageLogger.logError(s"$name is not are Procedure!");
          case None => MessageLogger.logError(s"$name is not defined");
        }
      case EmptyStatement =>
    }
  }


  def checkReferenceExpression(scope: Scope, referenceExpression: ReferenceExpression): Option[Symbol] = {
    referenceExpression match {
      case VariableReference(name) => scope.lookup(name)
      case ArrayAccess(reference, valueExpression) =>
        checkReferenceExpression(scope, reference) match {
          case Some(p@ParameterSymbol(_, typ, _)) =>
            if (typ.isInstanceOf[types.ArrayType]) {
              checkValueExpression(scope, valueExpression)
              Some(p)
            }
            else {
              MessageLogger.logError(s"Variable ist not an Array it is typ of $typ!")
              None
            }
          case Some(v@VariableSymbol(_, typ)) =>
            if (typ.isInstanceOf[types.ArrayType]) {
              checkValueExpression(scope, valueExpression)
              Some(v)
            }
            else {
              MessageLogger.logError(s"Variable ist not an Array it is typ of $typ!")
              None
            }
          case Some(symbol) => MessageLogger.logError(s"Illegal ReferenceExpression!"); None
          case None => MessageLogger.logError(s"Illegal ReferenceExpression!"); None
        }
    }
  }

  def checkAndGetTyp(scope: Scope, referenceExpression: ReferenceExpression): Type = {
    referenceExpression match {
      case VariableReference(name) => scope.lookup(name) match {
        case Some(ParameterSymbol(_, typ, _)) => typ
        case Some(VariableSymbol(_, typ)) => typ
        case Some(ProcedureSymbol(_, _, _)) => MessageLogger.logError(s"$name is a procedure and not a variable"); ErrorType
        case Some(TypeSymbol(_, _)) => MessageLogger.logError(s"$name is a typ and not a variable"); ErrorType
        case None => MessageLogger.logError(s"$name is not defined"); ErrorType
      }
      case ArrayAccess(reference, valueExpression) => {
        checkValueExpression(scope, valueExpression) match {
          case IntegerType =>
          case _ => return ErrorType
        }
        val baseType = checkAndGetTyp(scope, reference)
        baseType match {
          case ArrayType(base, _) => base
          case typ => MessageLogger.logError(s"Illegal typ found ($typ), but expected array!"); ErrorType
        }
      }
      case _ => ErrorType
    }
  }

  def checkValueExpression(scope: Scope, valueExpression: ValueExpression): Type = {
    valueExpression match {
      case Add(left, right) => checkArithExpTyp(scope, left, right)
      case Sub(left, right) => checkArithExpTyp(scope, left, right)
      case Mul(left, right) => checkArithExpTyp(scope, left, right)
      case Div(left, right) => checkArithExpTyp(scope, left, right)
      case UnaryMinus(right) => checkValueExpression(scope, right)
      case IntegerLiteral(_) => IntegerType

      case Less(left, right) => checkBoolExpTyp(scope, left, right)
      case LessEquals(left, right) => checkBoolExpTyp(scope, left, right)
      case Greater(left, right) => checkBoolExpTyp(scope, left, right)
      case GreaterEquals(left, right) => checkBoolExpTyp(scope, left, right)
      case Equals(left, right) => checkBoolExpTyp(scope, left, right)
      case NotEquals(left, right) => checkBoolExpTyp(scope, left, right)

      case Dereference(reference) => checkAndGetTyp(scope, reference)
    }
  }

  def checkArithExpTyp(scope: Scope, left: ValueExpression, right: ValueExpression): Type = {
    (checkValueExpression(scope, left), checkValueExpression(scope, right)) match {
      case (IntegerType, IntegerType) => IntegerType
      case _ => MessageLogger.logError(s"$left or $right are not of typ of int"); ErrorType
    }
  }

  def checkBoolExpTyp(scope: Scope, left: ValueExpression, right: ValueExpression): Type = {
    (checkValueExpression(scope, left), checkValueExpression(scope, right)) match {
      case (IntegerType, IntegerType) => BoolType
      case _ => MessageLogger.logError(s"$left or $right are not of typ of int"); ErrorType
    }
  }


  def conversionExpressionToTyp(typeExpression: TypeExpression): types.Type = typeExpression match {
    case ArrayTypeExpression(base, size) => ArrayType(conversionExpressionToTyp(base), size.integer)
    case NamedTypeExpression(symbol) => symbolTable.lookup(symbol) match {
      case Some(TypeSymbol(_, typ)) => typ
      case Some(sym) => MessageLogger.logError(s"$sym is not a typ"); ErrorType
      case None => MessageLogger.logError(s"""Typ with name $symbol not exists"""); ErrorType
    }
  }


  def clear(): Unit = {

  }

}

