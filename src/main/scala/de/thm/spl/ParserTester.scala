package de.thm.spl


import de.thm.spl.Parser.SPLParser

import scala.io.Source

object ParserTester {
  private val folder : String = "./parser/"

  def main(args: Array[String]): Unit = {
    val files =  List("exp.spl", "procs.spl", "stmts.spl", "types.spl", "queens.spl")

    files.foreach(testParser)
  }

  private def testParser(file: String): Unit ={
    printf("Test Parser with %s\n", file)
    val source  = Source.fromFile(folder + file)
    val code    = try source.mkString finally source.close()

    println(code)
    SPLParser(code)
    println("Test Parser end ... \n\n\n\n")

  }

  private def testSumme(): Unit ={
    val source = Source.fromFile("./progs/")
  }
}
