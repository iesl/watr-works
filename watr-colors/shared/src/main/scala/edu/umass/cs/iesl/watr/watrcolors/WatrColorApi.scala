package edu.umass.cs.iesl.watr
package watrcolors


trait WatrColorApi {
  def list(path: String): Seq[String]
}
