package de.thm.spl.symbols

trait Scope {
  def lookup(name: String): Option[Symbol]
  def define(symbol: Symbol): Unit
  def childScope(name: String): Option[Scope]
  def parent: Option[Scope]
}

