package edu.umass.cs.iesl.watr
package ext

import java.security.MessageDigest
import scalaz.@@

object DigestUtils {

  def shaHex(bytes: Array[Byte]): String@@SHA1String = {
    val digest = MessageDigest.getInstance("SHA")
    val digestBytes = digest.digest(bytes)

    hexEncode(digestBytes.toList)
  }

  def hexEncode(bytes: List[Byte]): String@@SHA1String = {
    SHA1String(bytes.map {
      b => String.format("%02X", java.lang.Byte.valueOf(b))
    }.mkString("").toLowerCase)
  }
}
