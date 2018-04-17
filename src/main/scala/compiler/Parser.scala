package compiler

import scala.io.Source
import scala.util.parsing.input.Reader
import compiler.frontend.ProgScanner

object Parser {


  def main(args: Array[String]): Unit = {
    val source  = Source.fromFile("prog.spl")
    val code    = try source.mkString finally source.close()
    val lexical : ProgScanner = new ProgScanner
    println("Parser started")

    val lexer: Reader[lexical.Token] = new lexical.Scanner(code.trim.stripSuffix(lexical.whitespacePattern))

    println(lexer.first)
  }

}
