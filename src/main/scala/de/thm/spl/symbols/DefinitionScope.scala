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
          case ProcedureSymbol(name, parameters, variables) =>
            childrenScopes += (name -> new DefinitionScope(Some(this)))
            childScope(name) match {
              case Some(scope) =>
                parameters.foreach(scope.define)
                variables.foreach(scope.define)
            }
          case _ =>
        }
    }
  }
  override def childScope(name: String): Option[Scope] = childrenScopes.get(name)

}
