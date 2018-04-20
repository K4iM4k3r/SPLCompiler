package de.thm.spl

import de.thm.spl.Parser.{SPLParser, SPLScanner, Tokens}

import scala.io.Source
import scala.util.parsing.input.Reader

object LexerTester {
  def main(args: Array[String]): Unit = {

    testAllToken()

    testParser()
  }

  private def testParser(): Unit ={
    println("Test Parser\n")
    val source  = Source.fromFile("array.spl")
    var code    = try source.mkString finally source.close()
    code = code.replaceAll("""\/\/.*\n""", "")

    println(code)
    println(SPLParser(code))
  }

  /*
    Test All Token
   */
  private def testAllToken(): Unit ={
    val source  = Source.fromFile("allToken.spl")
    var code    = try source.mkString finally source.close()
    var lines   = code.split("\n")
    println("Test AllToken started ... \n")

    code = code.replaceAll("""\/\/.*\n""", "")
    val scanner: Reader[Tokens.Token] = SPLScanner(code.trim.stripSuffix("""\s+"""))
    var tmp_scanner: Reader[Tokens.Token] = scanner

    lines.foreach(s => {
      printf("in: %s Token: %s\n",s, tmp_scanner.first)
      tmp_scanner = tmp_scanner.rest
    })
  }
}
