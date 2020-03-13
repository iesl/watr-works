package org.watrworks
package utils

import _root_.io.circe
import circe._
import circe.literal._
import circe.{parser => CirceParser}

object DoOrDieHandlers {

  private def dieImpl(msg: String = "")(
    srcName: sourcecode.Name,
    srcFile: sourcecode.File,
    srcLine: sourcecode.Line
  ): Nothing = {
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

  def die(msg: String = "")(implicit
    srcName: sourcecode.Name,
    srcFile: sourcecode.File,
    srcLine: sourcecode.Line
  ): Nothing = {
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

  implicit class RicherOption[A](val self: Option[A]) extends AnyVal {

    def orDie(msg: String = "")(implicit
      srcName: sourcecode.Name,
      srcFile: sourcecode.File,
      srcLine: sourcecode.Line
    ): A = {
      self.getOrElse {
        dieImpl(msg)(srcName, srcFile, srcLine)
      }
    }
  }

  implicit class DoOrDieHandlers_RicherString(val self: String) extends AnyVal {
    def jsonOrDie(msg: String = "")(implicit
      srcName: sourcecode.Name,
      srcFile: sourcecode.File,
      srcLine: sourcecode.Line
    ): Json = {
      CirceParser.parse(self) match {
        case Left(failure) => die(s"Invalid JSON String: (${msg}) ${failure}, string was ${self}")
        case Right(json) =>  json
      }
    }

    def decodeOrDie[T: Decoder](msg: String = "")(implicit
      srcName: sourcecode.Name,
      srcFile: sourcecode.File,
      srcLine: sourcecode.Line
    ): T = {
      CirceParser.parse(self) match {
        case Left(failure) => die(s"Invalid JSON String: (${msg}) ${failure}, string was ${self}")
        case Right(json) =>
          json.decodeOrDie[T]()
      }
    }
  }


  implicit class RicherJson(val self: circe.Json) extends AnyVal {

    def decodeOrDie[T: Decoder](msg: String = "")(implicit
      srcName: sourcecode.Name,
      srcFile: sourcecode.File,
      srcLine: sourcecode.Line
    ): T = {
      Decoder[T].decodeJson(self)
        .fold(
          fail => dieImpl(s"Decoding Json: ${msg} ${fail} on ${self}")(srcName, srcFile, srcLine),
          succ => succ
        )
    }
  }
}
