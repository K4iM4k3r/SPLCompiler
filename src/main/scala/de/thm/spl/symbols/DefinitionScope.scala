package de.thm.spl.symbols

class DefinitionScope(override val parent: Option[Scope]) extends Scope {
  private var definitions: Map[String, Symbol] = Map()
  private var childrenScopes: Map[String, DefinitionScope] = Map()

  override def lookup(name: String): Option[Symbol] = definitions.get(name).orElse(parent.flatMap(_.lookup(name)))
  override def define(symbol: Symbol): Unit = {
    definitions += (symbol.name -> symbol)
    symbol match {
      case ProcedureSymbol(name, _, _) => childrenScopes += (name -> new DefinitionScope(Some(this)))
      case _ =>
    }
  }
  override def childScope(name: String): Option[Scope] = childrenScopes.get(name)

}
