package de.thm.spl

import java.io.{BufferedWriter, File, FileWriter}

import de.thm.spl.Parser.SPLParser
import de.thm.{MessageLogger, puck}

object Compiler {
  case class Options(input: Option[String], output: Option[String], optimize: Boolean) {
    def isValid: Boolean = input.isDefined && output.isDefined
  }

  def apply(options: Options): Boolean = {
    val source = io.Source.fromFile(options.input.get).mkString
    val ast = SPLParser(source)

    println(MessageLogger)
    if (MessageLogger.hasErrorHappened || ast.isEmpty) return false

    val analyzedSyntax = ContextAnalysis(ast.get)

    println(MessageLogger)
    if (MessageLogger.hasErrorHappened) return false

    val generatedCode = CodeGenerator(analyzedSyntax)

    println(MessageLogger)
    if (MessageLogger.hasErrorHappened) return false

    val asmCode = puck.assembler.AbstractSyntaxPrinter(generatedCode)

    val bw = new BufferedWriter(new FileWriter(new File(options.output.get)))
    bw.write(asmCode)
    bw.close()

    println(asmCode)

    true
  }
}
