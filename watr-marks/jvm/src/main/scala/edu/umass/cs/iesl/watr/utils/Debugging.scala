package edu.umass.cs.iesl.watr
package utils


object Debugging {

  // pprint.pprintln(t: T, width: Integer, height: Integer, indent: Integer, colors: Colors)

  def log[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Name) = {
    println(enclosing.value + " [" + value.source + "]: " + value.value)
  }

  def die(t: Throwable)(implicit enclosing: sourcecode.File, line: sourcecode.Line): Unit = {
    val message = s"""error ${enclosing.value}:${line.value}: ${t}: ${t.getCause}: ${t.getMessage} """
    println(s"ERROR: ${message}")
    t.printStackTrace()
    throw t
  }

  def printAndSwallow(t: Throwable)(implicit enclosing: sourcecode.File, line: sourcecode.Line): Unit = {
    val message = s"""error ${enclosing.value}:${line.value}: ${t}: ${t.getCause}: ${t.getMessage} """
    println(s"ERROR: ${message}")
    t.printStackTrace()
  }

}

