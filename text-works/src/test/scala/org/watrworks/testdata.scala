package org.watrworks

import java.io.InputStream
import java.net.URL

object papers {
  def paper(filename: String): InputStream  = getClass().getResourceAsStream(s"/papers/$filename")
  def paperUrl(filename: String): URL  = getClass().getResource(s"/papers/$filename")
}
