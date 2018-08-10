package de.thm.spl

import de.thm.MessageLogger
import de.thm.puck.assembler.AbstractSyntax
import de.thm.puck.assembler.AbstractSyntax.{Add, _}
import de.thm.spl.symbols.{DefinitionScope, ParameterSymbol, ProcedureSymbol, SymbolTable}
import de.thm.spl.syntax.Definitions.ProcedureDefinition
import de.thm.spl.syntax.Expressions._
import de.thm.spl.syntax.Statements._
import de.thm.spl.syntax._
import de.thm.spl.types.ArrayType

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
  var labelCount: Int = -1

  def newLabel: Label = {
    labelCount = labelCount + 1

    Label("L_" + labelCount)
  }


  def getTempLocation: Int = {
    if (registers.isEmpty) {
      MessageLogger.logError("To many registers are used")
      println(MessageLogger)
      System.exit(-1)
    }
    val loc: Int = registers.head
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
    code += ImportDirective("spllib", "indexError", "")

    ast.definitions.foreach {
      case p@ProcedureDefinition(_, _, _, _) => generateCodeProcedure(p)
      case _ =>
    }


    def generateCodeProcedure(procedure: ProcedureDefinition): Unit = {
      val scope = symbolTable.childScope(procedure.name).get
      val label: Label = Label(procedure.name)
      val proc_vars: Long = symbolTable.getOffset(procedure.name)
      val proc_offset: Long = proc_vars + 4
      val proc_maxArgs: Int = symbolTable.getMaxArguments(procedure.name)
      val proc_numCalls: Int = symbolTable.getNumCalls(procedure.name)
      var additional_offset = 0


      if (procedure.name.equals("main")) code += ExecutableDirective(label)

      // Label add
      code += Label(procedure.name)

      // prologue
      code += Comment("procedure prologue")

      if (proc_vars > 0) {
        code += Comment(s"""allocate space for local variables ($proc_vars bytes)""")
        code += Subc(STACK, STACK, proc_vars)
      }

      code += Comment("save old frame pointer (4 bytes)")
      code += Subc(STACK, STACK, 4)
      code += Stw(FP, STACK)

      code += Comment("set new frame pointer")
      code += Addc(FP, STACK, proc_offset)

      if (proc_numCalls > 0) {
        code += Comment(s"""save return register (${procedure.name} calls at least one procedure in its body)""")
        code += Subc(STACK, STACK, 4)
        code += Stw(RETURN, STACK)

        additional_offset = 4

        if (proc_maxArgs > 0) {
          code += Comment("allocate space for outgoing arguments ("+ proc_maxArgs * 4 +" bytes)")
          code += Subc(STACK, STACK, proc_maxArgs * 4)
        }
      }



      // Body
      code += Comment("procedure body")
      procedure.body.foreach(generateCodeCmd)


      // epilogue
      code += Comment("procedure epilogue")

      if (proc_numCalls > 0) {
        code += Comment("restore return register")
        code += Addc(STACK, STACK, proc_maxArgs * 4)
        code += Ldw(RETURN, STACK)
        code += Addc(STACK, STACK, 4)
      }

      code += Comment("restore old frame pointer")
      code += Ldw(FP, STACK)
      code += Addc(STACK, STACK, proc_offset)

      code += Comment("return to caller")
      code += Jmpr(RETURN)

      def generateTargetAddress(ref: ReferenceExpression, target: Int): Unit = {
        ref match {
          case VariableReference(name) =>
            val offset: Long = scope.getOffset(name)

            if (offset < 0) {
              code += Subc(target, FP, -offset)
            }
            else {
              code += Addc(target, FP, offset)
            }
            scope.lookup(name).get match {
              case ParameterSymbol(_, _, isRef, _) => if (isRef) {
                code += Ldw(target, target)
              }
              case _ => None
            }

          case a@ArrayAccess(reference, valueExpression) =>
            generateTargetAddress(reference, target)

            val t1 = getTempLocation
            val t2 = getTempLocation

            code += Comment("generating index")
            generateCodeValueExp(valueExpression, t1)

            code += Comment("boundary check")
            reference.getType(scope) match {
              case ArrayType(_, size, _) =>
                code += Setw(t2, Left(size))
                code += Ltu(t2, t1, t2)
                code += Brf(t2, Label("indexError"))
              case _ =>
            }

            code += Setw(t2, Left(a.getType(scope).byteSize))

            code += Mulu(t1, t1, t2)
            code += Add(target, target, t1)

            freeTempLocation(t2)
            freeTempLocation(t1)
        }
      }


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

          generateCodeBool(condition, thenLabel, brf = true)
          if (!elseCmd.equals(EmptyStatement)) {
            generateCodeCmd(thenCmd)
            code += Jmp(exitLabel)
            code += thenLabel
            generateCodeCmd(elseCmd)
            code += exitLabel
          }
          else {
            generateCodeCmd(thenCmd)
            code += thenLabel

          }
        case While(condition, body) =>
          val startLabel = newLabel
          val continueLabel = newLabel

          code += Jmp(startLabel)
          code += continueLabel
          generateCodeCmd(body)

          code += startLabel
          generateCodeBool(condition, continueLabel, brf = false)


        case CompoundStatement(body) => body.foreach(generateCodeCmd)

        case Statements.Call(name, arguments) =>

          var counter: Int = 0
          symbolTable.lookup(name).get match {
            case ProcedureSymbol(_, parameters, _, _, _, _) =>

              arguments.zip(parameters).foreach(tup => {
                code += Comment(s"""storing argument #$counter for procedure $name""")
                val t1 = getTempLocation

                tup match {
                  case (d@Dereference(ref), param) =>


                    if (param.isRef) {
                      generateTargetAddress(ref, t1)
                    }
                    else {
                      generateCodeValueExp(d, t1)
                    }

                  case (arg, _) =>

                    generateCodeValueExp(arg, t1)

                  case _ =>

                }
                val offset = proc_offset + 4 + (proc_maxArgs * 4) - (counter*4)
                code += Subc(FP, FP, offset)
                code += Stw(t1, FP)
                code += Addc(FP, FP, offset)


                freeTempLocation(t1)
                counter += 1
              })
            case _ =>

          }


          // Sprung zu Proc
          code += Comment(s"""call procedure $name""")
          code += AbstractSyntax.Call(RETURN, Label(name))

        case EmptyStatement => code += NoOp

      }


      def generateCodeValueExp(exp: ValueExpression, target: Int): Unit = exp match {
        case Expressions.Add(l, r) => genBinOp(l, AddOp, r, target)
        case Expressions.Sub(l, r) => genBinOp(l, SubOp, r, target)
        case Expressions.Mul(l, r) => genBinOp(l, MulOp, r, target)
        case Expressions.Div(l, r) => genBinOp(l, DivOp, r, target)

        case Expressions.UnaryMinus(r) =>
          generateCodeValueExp(r, target)
          code += AbstractSyntax.Sub(target, 0, target)
          freeTempLocation(target)
        case Expressions.IntegerLiteral(l) =>
          code += Setw(target, Left(l))

        case Dereference(reference) =>
          reference match {
            case VariableReference(name) =>
              val offset: Long = scope.getOffset(name)
              if (offset < 0) {
                code += Subc(target, FP, -offset)
              }
              else {
                code += Addc(target, FP, offset)
              }
              code += Ldw(target, target)

              // If Ref ignore Dereferenz
              scope.lookup(name).get match {
                case ParameterSymbol(_, _, isRef, _) => if (isRef) {
                  code += Ldw(target, target)
                }
                case _ => None
              }

            case a@ArrayAccess(ref, index) =>

              generateTargetAddress(ref, target)

              val t1 = getTempLocation
              code += Comment("generating index")
              generateCodeValueExp(index, t1)

              val t2 = getTempLocation

              code += Comment("boundary check")
              val c = ref.getType(scope)
              c match {
                case ArrayType(_, size, _) =>
                  code += Setw(t2, Left(size))
                  code += Ltu(t2, t1, t2)
                  code += Brf(t2, Label("indexError"))
                case _ =>
              }

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
        
        generateCodeValueExp(l, target)
        generateCodeValueExp(r, t1)

        op match {
          case AddOp =>
            code += AbstractSyntax.Add(target, target, t1)
          case SubOp =>
            code += AbstractSyntax.Sub(target, target, t1)
          case MulOp =>
            code += AbstractSyntax.Mulu(target, target, t1)
          case DivOp =>
            code += AbstractSyntax.Divi(target, target, t1)
        }

        freeTempLocation(t1)
      }

      def generateCodeBool(bExp: ValueExpression, falseLabel: Label, brf: Boolean): Unit = {

        def genCondJump(l: ValueExpression, r: ValueExpression, op: Op): Unit = {
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

          if (brf) code += Brf(t1, falseLabel)
          else code += Brt(t1, falseLabel)

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

    }
    // Offsets in register rein nicht mit addc weil nur ein Byte
    code.toList
  }
}