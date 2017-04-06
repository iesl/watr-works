package edu.umass.cs.iesl.watr.utils


object Debugging {

  // pprint.pprintln(t: T, width: Integer, height: Integer, indent: Integer, colors: Colors)

  def log[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Name) = {
    println(enclosing.value + " [" + value.source + "]: " + value.value)
  }

}
