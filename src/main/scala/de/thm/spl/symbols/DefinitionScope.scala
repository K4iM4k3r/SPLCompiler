package de.thm.spl.symbols

import de.thm.MessageLogger

class DefinitionScope(override val parent: Option[Scope]) extends Scope {
  private var definitions: Map[String, Symbol] = Map()
  private var childrenScopes: Map[String, DefinitionScope] = Map()

  override def lookup(name: String): Option[Symbol] = {
    definitions.get(name).orElse(parent.flatMap(_.lookup(name)))
  }

  override def define(symbol: Symbol): Unit = {
    lookup(symbol.name) match{
      case Some(_) => MessageLogger.logError(s"""$symbol already defined""")
      case None    =>
        definitions += (symbol.name -> symbol)
        symbol match {
          case ProcedureSymbol(name, parameters, variables, _, _,_) =>
            childrenScopes += (name -> new DefinitionScope(Some(this)))
            childScope(name) match {
              case Some(scope) =>
                parameters.foreach(scope.define)
                variables.foreach(scope.define)
              case _ => MessageLogger.logError("Childscope not found")
            }
          case _ =>
        }
    }
  }
  override def childScope(name: String): Option[DefinitionScope] = childrenScopes.get(name)


  def getOffset(name: String): Long = {
    lookup(name).get match {
      case VariableSymbol(_,_, offset) => offset
      case ParameterSymbol(_,_,_,offset) => offset
      case ProcedureSymbol(_,_,_, offset, _,_) => offset
      case _ => 0
    }
  }

  def setMaxArguments(name: String, value: Int): Unit ={
    lookup(name).get match {
      case p@ProcedureSymbol(_,_,_,_,_,_) => p.maxArgs = value
    }
  }

  def getMaxArguments(name: String): Int ={
    lookup(name).get match {
      case ProcedureSymbol(_,_,_, _, args,_) => args
      case _ => 0
    }
  }

  def setNumCalls(name: String, value: Int): Unit ={
    lookup(name).get match {
      case p@ProcedureSymbol(_,_,_,_,_,_) => p.numCalls = value
    }
  }


  def getNumCalls(name: String): Int ={
    lookup(name).get match {
      case ProcedureSymbol(_,_,_, _, _, num) => num
      case _ => 0
    }
  }
}
