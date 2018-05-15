package de.thm.spl

import de.thm.MessageLogger
import de.thm.spl.Parser.SPLParser
import de.thm.spl.syntax.Definitions._

import scala.io.Source

object ContextTester{

    def main(args: Array[String]): Unit = {
//      val files =  List("acker.spl", "arrayasgn.spl", "arrayparm.spl", "bits.spl", "queens.spl", "gcd.spl", "multiply.spl", "partind1.spl", "partind2.spl", "reftest.spl", "sierpinski.spl", "swap.spl", "" +
//        "test1.spl", "test2.spl", "test3.spl", "test4.spl", "test5.spl", "test6.spl", "test7.spl", "test8.spl", "test9.spl", "threedim.spl", "time.spl", "twodim.spl")
//
//      files.foreach(testContext)

      testContext("partind1.spl")
    }



    def testContext(prog: String): Unit = {
      val folder = "/home/kai/Downloads/reftests/"
      printf("Test Context with %s\n", prog)
      val source = Source.fromFile(folder + prog)
      val code = try source.mkString finally source.close()

      MessageLogger.clear()
      val ast = SPLParser(code)
      println(ast)
      println(MessageLogger)

      if (!MessageLogger.hasErrorHappened) {
        println("Context Analysis started")
        ContextAnalysis(ast.get)
        println(MessageLogger)
      }
      println("\n")
    }


















//    val source  = Source.fromFile("./parser/queens.spl")
//    var code    = try source.mkString finally source.close()
//
////    println(code)
//
//
//    }
//
//


}
