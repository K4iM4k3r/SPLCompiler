package de.thm.spl

import de.thm.MessageLogger
import de.thm.spl.Parser.SPLParser
import de.thm.spl.syntax.Definitions._
import de.thm.spl.syntax.Statement
import de.thm.spl.syntax.Statements.{CompoundStatement, If, While}

import scala.io.Source

object ContextTester{
   final val SPACE: String = "     "

    def main(args: Array[String]): Unit = {
//      val files =  List("acker.spl", "arrayasgn.spl", "arrayparm.spl", "bits.spl", "queens.spl", "gcd.spl", "multiply.spl", "partind1.spl", "partind2.spl", "reftest.spl", "sierpinski.spl", "swap.spl", "" +
//        "test1.spl", "test2.spl", "test3.spl", "test4.spl", "test5.spl", "test6.spl", "test7.spl", "test8.spl", "test9.spl", "threedim.spl", "time.spl", "twodim.spl")
//
//      files.foreach(testContext)

      testContext("queens.spl")
    }



    def testContext(prog: String): Unit = {
      val folder = "/home/kai/Downloads/reftests/"
      printf("Test Context with %s\n", prog)
      val source = Source.fromFile(folder + prog)
      val code = try source.mkString finally source.close()

      MessageLogger.clear()
      val ast = SPLParser(code)
//      println(ast)
      printAst(ast.get)
      println(MessageLogger)

      if (!MessageLogger.hasErrorHappened) {
        println("Context Analysis started")
        ContextAnalysis(ast.get)
        println(MessageLogger)
      }
      println("\n")
    }


  def printAst(ast: syntax.Program): Unit ={
    println("Program( ")
    println(ast.definitions.map{
      case ProcedureDefinition(name, params, vars, body) =>
        var proc: String = SPACE + "ProcedureDefinition(" + name + ",\n"

        if(params.nonEmpty) proc += SPACE * 2 + params.mkString(",\n"+SPACE * 2) + ",\n"
        if(vars.nonEmpty)   proc += SPACE * 2 + vars.mkString(",\n"+SPACE * 2) + ",\n"

        proc += body.map(f => styleStmt(3, f) ).mkString(",\n")
        proc += "\n"+ SPACE +")"
        proc
      case d => SPACE + d.toString
    }.mkString(", \n"))
  }

  def styleStmt(deepth: Int, statement: Statement): String = {
    val dSPACE: String = SPACE * deepth
    statement match {
      case While(condition, body) =>
        dSPACE + "While(" + condition.toString + ",\n" + styleStmt(deepth + 1, body) + "\n" + dSPACE + ")"
      case If(condition, thenStatements, elseStatements) =>
        dSPACE + "If(" + condition.toString + ",\n" + styleStmt(deepth + 1, thenStatements) + "\n" + dSPACE + ",\n" + styleStmt(deepth + 1, elseStatements) + "\n" + dSPACE + ")"
      case CompoundStatement(body) =>
        dSPACE + "CompoundStatement(\n" +  body.map(f => styleStmt(deepth + 1, f) ).mkString(",\n") + "\n" + dSPACE + ")"
      case stmt =>
        dSPACE + stmt.toString

    }
  }

}
