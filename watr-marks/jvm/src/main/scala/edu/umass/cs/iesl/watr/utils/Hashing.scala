package edu.umass.cs.iesl.watr
package utils

import java.security.MessageDigest
import scalaz.@@
//import TypeTags._

object Hashing {

  def toSHA1String(s: String): String@@SHA1String = {
    hexEncode(digestBytes(s.getBytes))
  }

  private def hexEncode(bytes: Seq[Byte]): String@@SHA1String = {
    SHA1String(bytes.map {
      b => String.format("%02X", java.lang.Byte.valueOf(b))
    }.mkString("").toLowerCase)
  }

  lazy val digest = MessageDigest.getInstance("SHA")
  private def digestBytes(bytes: Array[Byte]): Array[Byte] = {
    digest.digest(bytes)
  }


}
