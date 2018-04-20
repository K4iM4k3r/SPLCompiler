package de.thm.spl

import de.thm.spl.Parser.{SPLParser, SPLScanner, Tokens}

import scala.io.Source
import scala.util.parsing.input.Reader

object LexerTester {
  def main(args: Array[String]): Unit = {
    var files =  List("array.spl", "Summe.spl", "proc.spl", "fac_iter.spl", "fac_rec.spl")

    testAllToken()

    files.foreach(testParser)
  }

  private def testParser(file: String): Unit ={
    printf("Test Parser with %s\n", file)
    val source  = Source.fromFile(file)
    var code    = try source.mkString finally source.close()
    code = code.replaceAll("""\/\/.*\n""", "")

    println(code)
    SPLParser(code)
    println("Test Parser end ... \n\n\n\n")

  }

  /*
    Test All Token
   */
  private def testAllToken(): Unit ={
    val source  = Source.fromFile("allToken.spl")
    var code    = try source.mkString finally source.close()
    val lines   = code.split("\n")
    println("Test AllToken started ... \n")

    code = code.replaceAll("""\/\/.*\n""", "")
    val scanner: Reader[Tokens.Token] = SPLScanner(code.trim.stripSuffix("""\s+"""))
    var tmp_scanner: Reader[Tokens.Token] = scanner

    lines.foreach(s => {
      printf("in: %s Token: %s\n",s, tmp_scanner.first)
      tmp_scanner = tmp_scanner.rest
    })
    println("Test AllToken end ... \n\n\n\n")

  }
}
