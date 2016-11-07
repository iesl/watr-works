package edu.umass.cs.iesl.watr
package utils

object PathUtils {


  def appendTimestamp(path: String): String = {
    import java.text.SimpleDateFormat
    import java.util.Date
    val dateStamp = new SimpleDateFormat("yyyyMMddhhmmss").format(new Date())
    s"$path-$dateStamp"
  }

}
