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
  var counterMaxArg: Int = 0
  var counterCalls: Int = 0

  def apply(abstractSyntax: syntax.Program): SymbolTable = {
    val storedProcedure: ListBuffer[ProcedureDefinition] = new ListBuffer[ProcedureDefinition]
    symbolTable = new SymbolTable
    // Default typ and library functions
    initSymbolTable()

    // Definition of Types and Procedures
    abstractSyntax.definitions.foreach {
      case TypeDefinition(name, typexp) =>
        symbolTable.define(TypeSymbol(name, conversionExpressionToTyp(typexp)))
      case proc@ProcedureDefinition(name, params, variables, body) =>
        storedProcedure += proc
        var offset_param: Long = 0
        var offset_vars: Long = 0

        symbolTable.define(ProcedureSymbol(name, params.map(p => {
          offset_param += 4
          val typParam = conversionExpressionToTyp(p.typExp)
          typParam match {
            case ArrayType(_,_,_) if !p.isRef => MessageLogger.logError(s"""parameter '${p.name}' must be a reference parameter""")
            case _ =>
          }
          ParameterSymbol(p.name, typParam, p.isRef, offset_param-4)
        }), variables.map(v => {
          offset_vars -= calcSize(v.typeExp)
          val symbol = VariableSymbol(v.name, conversionExpressionToTyp(v.typeExp), offset_vars)

          symbol
        }), -offset_vars, 0, 0))

    }

    //checking of Variable, Parameter usage
    storedProcedure.foreach(p => {
      val scope: Scope = symbolTable.childScope(p.name).get
      // Reset of counters
      counterCalls = 0
      counterMaxArg = 0

      p.body.foreach(s => checkStatement(scope, s))

      symbolTable.setMaxArguments(p.name, counterMaxArg)
      symbolTable.setNumCalls(p.name, counterCalls)
    })

    // Test if main proc exists
    if (symbolTable.childScope("main").isEmpty) MessageLogger.logError("main proc is missing")

    symbolTable
  }

  def initSymbolTable(): Unit = {
    symbolTable.define(TypeSymbol("int", IntegerType))
    symbolTable.define(ProcedureSymbol("printi", List(ParameterSymbol("i", IntegerType, isRef = false, 4)), List(), 0, 0, 0))
    symbolTable.define(ProcedureSymbol("printc", List(ParameterSymbol("i", IntegerType, isRef = false, 4)), List(), 0, 0, 0))
    symbolTable.define(ProcedureSymbol("readi", List(ParameterSymbol("i", IntegerType, isRef = true, 4)), List(), 0, 0, 0))
    symbolTable.define(ProcedureSymbol("readc", List(ParameterSymbol("i", IntegerType, isRef = true, 4)), List(), 0, 0, 0))
    symbolTable.define(ProcedureSymbol("exit", List(), List(), 0, 0, 0))
    symbolTable.define(ProcedureSymbol("time", List(ParameterSymbol("i", IntegerType, isRef = true, 4)), List(), 0, 0, 0))

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
        // update Counter
        counterCalls += 1

        if(counterMaxArg < arguments.length){
          counterMaxArg =  arguments.length
        }

        symbolTable.lookup(name) match {
          case Some(p@ProcedureSymbol(_, _, _, _,_,_)) =>
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
          case Some(p@ParameterSymbol(_, typ, _, _)) =>
            if (typ.isInstanceOf[types.ArrayType]) {
              checkValueExpression(scope, valueExpression)
              Some(p)
            }
            else {
              MessageLogger.logError(s"Variable ist not an Array it is typ of $typ!")
              None
            }
          case Some(v@VariableSymbol(_, typ, _)) =>
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
        case Some(ParameterSymbol(_, typ, _, _)) => typ
        case Some(VariableSymbol(_, typ, _)) => typ
        case Some(ProcedureSymbol(_, _, _, _,_,_)) => MessageLogger.logError(s"$name is a procedure and not a variable"); ErrorType
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
          case ArrayType(base, _, _) => base
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
    case ArrayTypeExpression(base, size) =>
      val t = conversionExpressionToTyp(base)
      t match {
        case ArrayType(_, _, byteSize) => ArrayType(t, size.integer,byteSize * size.integer)
        case IntegerType =>  ArrayType(t, size.integer, size.integer * 4)
        case _ => MessageLogger.logError(s"""invalid base typ $base"""); ErrorType
      }
    case NamedTypeExpression(symbol) => symbolTable.lookup(symbol) match {
      case Some(TypeSymbol(_, typ)) => typ
      case Some(sym) => MessageLogger.logError(s"$sym is not a typ"); ErrorType
      case None => MessageLogger.logError(s"""Typ with name $symbol not exists"""); ErrorType
    }
  }


  def calcSize(typeExpression: TypeExpression): Long ={
    typeExpression match {
      case ArrayTypeExpression(base, size) =>
        val res = calcSize(base) * size.integer
        res
      case NamedTypeExpression(symbol) => symbolTable.lookup(symbol) match {
        case Some(TypeSymbol(_, typ)) => calc(typ)
        case _ => MessageLogger.logError(s"""symbol $symbol is not typesymbol"""); 0
      }
    }
  }

  def calc(typ: Type): Long ={
    typ match {
      case IntegerType => 4
      case ArrayType (base, size, _) => calc(base) * size
      case _ => MessageLogger.logError(s"""base typ $typ is not array or integer"""); 0
    }
  }

  def getMaxArgs(body: List[Statement]): Int ={
    body.map{
      case Call(_,arguments) => arguments.length
      case _ => -1
    }.max
  }

  def getNumCalls(body: List[Statement]): Int ={
    body.count{
      case Call(_,_) => true
      case _ =>  false
    }
  }
}

