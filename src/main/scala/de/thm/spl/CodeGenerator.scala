package de.thm.spl

import de.thm.MessageLogger
import de.thm.puck.assembler.AbstractSyntax
import de.thm.puck.assembler.AbstractSyntax.{Add, _}
import de.thm.spl.symbols.{DefinitionScope, SymbolTable}
import de.thm.spl.syntax.Definitions.ProcedureDefinition
import de.thm.spl.syntax.Expressions._
import de.thm.spl.syntax.Statements._
import de.thm.spl.syntax._

import scala.collection.mutable.ListBuffer

object CodeGenerator {
  abstract class Op
  case object EqOp extends Op
  case object NeOp extends Op
  case object LtOp extends Op
  case object GtOp extends Op
  case object LeOp extends Op
  case object GeOp extends Op
  case object AddOp extends Op
  case object SubOp extends Op
  case object MulOp extends Op
  case object DivOp extends Op


// Register 29 Stack
  val FP: Int = 29
  val STACK: Int = 31
  val RETURN: Int = 30
  var registers: List[Int] = (1 to 28).toList

  // Create labels
  var labelCount = 0
  def newLabel : Label = {
    labelCount = labelCount + 1

    Label("L_" + labelCount)
  }
  var scope: DefinitionScope = new DefinitionScope(None)

  def getTempLocation: Int ={
    if (registers.isEmpty) MessageLogger.logError("To many registers are used")
    val loc : Int = registers.head
    registers = registers.tail

    loc
  }

  def freeTempLocation(loc: Int): Unit = {
    registers = loc :: registers
  }


  def apply(ast: Program, symbolTable: SymbolTable, objectName: String): List[AssemblerLine] = {
    var code: ListBuffer[AssemblerLine] = ListBuffer()

    code += ObjectDirective(objectName)


    // import directives of stdlib
    code += ImportDirective("spllib", "printi", "")
    code += ImportDirective("spllib", "printc", "")
    code += ImportDirective("spllib", "readi", "")
    code += ImportDirective("spllib", "readc", "")
    code += ImportDirective("spllib", "exit", "")
//    code += ImportDirective("spllib", "time", "")

    ast.definitions.foreach {
      case p@ProcedureDefinition(_,_,_,_) => generateCodeProcedure(p)
    }

    def generateTargetAddress(ref: ReferenceExpression, target: Int): Unit = ref match {
      case VariableReference(name) =>
        val offset: Long =  scope.getOffset(name)
        if(offset < 0){
          code += Subc(target, FP, -offset)
        }
        else{
          code += Addc(target, FP, offset)
        }

      case a@ArrayAccess(reference, valueExpression) =>
        generateTargetAddress(reference, target)
        val t1 = getTempLocation

        generateCodeValueExp(valueExpression, t1)

        val t2 = getTempLocation

        code += Setw(t2, Left(a.getType(scope).byteSize))

        code += Mulu(t1, t1, t2)
        code += Add(target, target, t1)

        freeTempLocation(t2)
        freeTempLocation(t1)
    }

    def generateCodeProcedure(procedure: ProcedureDefinition): Unit = {
      scope = symbolTable.childScope(procedure.name).get
      val label: Label = Label(procedure.name)

      if(procedure.name.equals("main")) code += ExecutableDirective(label)

      // Label add
      code += Label(procedure.name)

      val proc_offset: Long = symbolTable.getOffset(procedure.name)
      // prologue
      code += Comment("procedure prologue")
      code += Subc(STACK, STACK, 4)
      code += Stw(FP, STACK)
      code += Cp(FP, STACK)
      code += Subc(STACK, STACK, proc_offset)


      // Body
      code += Comment("procedure body")
      procedure.body.foreach(generateCodeCmd)


      // epilogue
      code += Comment("procedure epilogue")
      code += Cp(STACK, FP)
      code += Ldw(FP, STACK)
      code += Addc(STACK, STACK, 4)
      code += Comment("return to caller")
      code += Jmpr(RETURN)


    def generateCodeCmd(cmd: Statement): Unit = cmd match {

        case Assignment(target, value) =>
          val address = getTempLocation
          generateTargetAddress(target, address)

          val t1 = getTempLocation
          generateCodeValueExp(value, t1)
          code += Stw(t1, address)

          freeTempLocation(t1)
          freeTempLocation(address)

        case If(condition, thenCmd, elseCmd) =>
          val thenLabel = newLabel
          val exitLabel = newLabel

          generateCodeBool(condition, thenLabel)
          if(!elseCmd.equals(EmptyStatement)){

            code += thenLabel
            generateCodeCmd(thenCmd)

            generateCodeCmd(elseCmd)
            code += Jmp(exitLabel)
          }

//          code += thenLabel

        case While(condition, body) =>
          val startLabel = newLabel
          val continueLabel = newLabel
          val endLabel = newLabel
          code += startLabel
          generateCodeBool(condition, continueLabel)
          code += Jmp(endLabel)
          code += continueLabel
          generateCodeCmd(body)
          code += Jmp(startLabel)
          code += endLabel

        case CompoundStatement(body) => body.foreach(generateCodeCmd)

        case Statements.Call(name, arguments) =>
          code += Subc(STACK, STACK, 4)
          code += Stw(RETURN, STACK)

          arguments.reverse.foreach(a => {
            val t1 = getTempLocation
            generateCodeValueExp(a, t1)
            code += Subc(STACK, STACK, 4)
            code += Stw(t1, STACK)

            freeTempLocation(t1)
          })




          // Sprung zu Proc
          code += AbstractSyntax.Call(RETURN, Label(name))
          //Stack wieder herstellen
          code += Addc(STACK, STACK, arguments.length * 4)
          code += Ldw(RETURN, STACK)
          code += Addc(STACK, STACK, 4)


        case  EmptyStatement => code += NoOp

      }
    }

    def generateCodeBool(bExp: ValueExpression, falseLabel: Label): Unit ={

      def genCondJump(l: ValueExpression, r: ValueExpression, op: Op) : Unit = {
        val t1 = getTempLocation
        generateCodeValueExp(l, t1)
        val t2 = getTempLocation
        generateCodeValueExp(r, t2)
        op match {
          case EqOp => code += Eq(t1, t1, t2)
          case NeOp => code += Ne(t1, t1, t2)
          case LtOp => code += Lti(t1, t1, t2)
          case GtOp => code += Gti(t1, t1, t2)
          case LeOp => code += Lei(t1, t1, t2)
          case GeOp => code += Gei(t1, t1, t2)
        }
        freeTempLocation(t2)

        code += Brf(t1, falseLabel)
        freeTempLocation(t1)

      }

      bExp match {
        case Less(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, LtOp)
        case Greater(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, GtOp)
        case Equals(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, EqOp)
        case LessEquals(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, LeOp)
        case GreaterEquals(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, GeOp)
        case NotEquals(l: ValueExpression, r: ValueExpression) =>
          genCondJump(l, r, NeOp)
      }
    }

    def generateCodeValueExp(exp: ValueExpression, target: Int): Unit = exp match {
      case Expressions.Add(l, r) => genBinOp(l, AddOp, r, target)
      case Expressions.Sub(l, r) => genBinOp(l, SubOp, r, target)
      case Expressions.Mul(l, r) => genBinOp(l, MulOp, r, target)
      case Expressions.Div(l, r) => genBinOp(l, DivOp, r, target)

      case Expressions.UnaryMinus(r) =>
        val t1 = getTempLocation
        generateCodeValueExp(r, t1)
        code += AbstractSyntax.Sub(target, 0, t1)
        freeTempLocation(t1)
      case Expressions.IntegerLiteral(l) =>
        code += Setw(target, Left(l))

      case Dereference(reference) =>
        reference match {
          case VariableReference(name) =>
            val offset: Long = scope.getOffset(name)
            if(offset < 0){
              code += Subc(target, FP, -offset)
            }
            else{
              code += Addc(target, FP, offset)
            }
            code += Ldw(target, target)

          case a@ArrayAccess(ref, index) =>
            code += Comment("adress")

            generateTargetAddress(ref, target)

            code += Comment("value")
            val t1 = getTempLocation

            generateCodeValueExp(index, t1)

            val t2 = getTempLocation
            //TODO
            code += Setw(t2, Left(a.getType(scope).byteSize))
            code += Mulu(t1, t1, t2)
            code += Add(target, target, t1)
            code += Ldw(target, target)

            freeTempLocation(t2)
            freeTempLocation(t1)
        }

    }

    def genBinOp(l: ValueExpression, op: Op, r: ValueExpression, target: Int): Unit = {
      val t1 = getTempLocation
      generateCodeValueExp(l, t1)
      val t2 = getTempLocation
      generateCodeValueExp(r, t2)

      op match {
        case AddOp =>
          code += AbstractSyntax.Add(target, t1, t2)
        case SubOp =>
          code += AbstractSyntax.Sub(target, t1, t2)
        case MulOp =>
          code += AbstractSyntax.Mulu(target, t1, t2)
        case DivOp =>
          code += AbstractSyntax.Divi(target, t1, t2)
      }

      freeTempLocation(t2)
      freeTempLocation(t1)
    }



    code.toList
  }






  // Offsets in register rein nicht mit addc weil nur ein Byte
}
