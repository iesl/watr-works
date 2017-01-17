package edu.umass.cs.iesl.watr
package extract 

import java.security.MessageDigest

//import TypeTags._

object Hash {

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
