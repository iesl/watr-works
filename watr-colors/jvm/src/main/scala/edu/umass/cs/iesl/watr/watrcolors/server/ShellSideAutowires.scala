package edu.umass.cs.iesl.watr
package watrcolors
package server


import upickle.{default => UPickle}

object ShellsideServer extends autowire.Server[String, UPickle.Reader, UPickle.Writer] {
  override def read[Result: UPickle.Reader](p: String) = UPickle.read[Result](p)
  override def write[Result: UPickle.Writer](r: Result) = UPickle.write(r)
}
