package de.thm.spl

import syntax.Program
import de.thm.puck
import de.thm.spl.symbols.SymbolTable

object CodeGenerator {
  def apply(ast: Program, symbolTable: SymbolTable): List[puck.assembler.AbstractSyntax.AssemblerLine] = ???
}
