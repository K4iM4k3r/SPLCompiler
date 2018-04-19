package de.thm

import scala.collection.mutable.ListBuffer

object MessageLogger {
  private val errors: ListBuffer[String] = ListBuffer()
  private val warnings: ListBuffer[String] = ListBuffer()

  def logWarning(msg: String): Unit = {
    warnings.append("Warning: " + msg)
  }

  def logError(msg: String): Unit = {
    errors.append("Error: " + msg)
  }

  def hasErrorHappened: Boolean = errors.nonEmpty

  override def toString: String = {
    //Adding one to line number to make is one based instead of zero based
    errors.fold("")((error: (String), msg: String) => {
      error + msg + '\n'
    }) +
      warnings.fold("")((error: (String), msg: String) => {
        error + msg + '\n'
      })
  }
}
