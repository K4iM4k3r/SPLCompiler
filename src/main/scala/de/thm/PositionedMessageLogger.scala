package de.thm

import scala.util.parsing.input.Position

object PositionedMessageLogger {
  def logWarning(line: Position, msg: String): Unit = {
    MessageLogger.logWarning("at " + line + ": " + msg)
  }

  def logError(line: Position, msg: String): Unit = {
    MessageLogger.logError("at " + line + ": " + msg)
  }

  override def toString: String = MessageLogger.toString
}
