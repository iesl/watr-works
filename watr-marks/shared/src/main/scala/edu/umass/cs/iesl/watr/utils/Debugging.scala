package edu.umass.cs.iesl.watr.utils


object Debugging {

  def log[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Name) = {
    println(enclosing.value + " [" + value.source + "]: " + value.value)
  }

}
