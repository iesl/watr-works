package edu.umass.cs.iesl.watr
package utils

object DoOrDieHandlers {

  implicit class RicherOption[A](val self: Option[A]) extends AnyVal {

    def orDie(msg: String = "")(implicit
      srcName: sourcecode.Name,
      srcFile: sourcecode.File,
      srcLine: sourcecode.Line
    ): A = {
      self.getOrElse {
        val n = srcName.value
        val f = srcFile.value.split("/").last
        val l = srcLine.value
        val message = if (msg.length()> 0) {
          s"""${msg}: ${n} line ${l} in ${f} """
        } else {
          s"""Unspecifed error: in ${n} line ${l} of ${f}"""
        }
        sys.error(message)
      }
    }
  }

}
