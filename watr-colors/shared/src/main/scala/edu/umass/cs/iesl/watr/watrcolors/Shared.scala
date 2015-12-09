package edu.umass.cs.iesl.watr
package watrcolors


trait Api{
  def list(path: String): Seq[String]
}
