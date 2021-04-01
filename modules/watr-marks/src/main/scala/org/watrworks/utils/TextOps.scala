package org.watrworks
package utils


object TextOps {
  def trimQuotes(s: String): String = {
    val t = s.trim
    if (t.length() >= 2) {
      val isQuote = List('"', '\'').contains(t.head)
      if (isQuote && t.head == t.last) {
        t.drop(1).dropRight(1)
      } else s
    } else s
  }


}
