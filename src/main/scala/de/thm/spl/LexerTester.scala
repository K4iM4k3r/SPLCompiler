package de.thm.spl

import de.thm.spl.Parser.{SPLScanner, Tokens}

import scala.io.Source
import scala.util.parsing.input.Reader

object LexerTester {
  def main(args: Array[String]): Unit = {
    val source  = Source.fromFile("array.spl")
    var code    = try source.mkString finally source.close()
    println("Scanner started ... \n")

    code = code.replaceAll("""\/\/.*\n""", "")
    val scanner: Reader[Tokens.Token] = SPLScanner(code.trim.stripSuffix("""\s+"""))

    var tmp_scanner: Reader[Tokens.Token] = scanner
    while(!tmp_scanner.atEnd){
      println(tmp_scanner.first)
      tmp_scanner = tmp_scanner.rest
    }
    println("... Scanner stopped\n")

  }
}
