package org.watrworks


object papers {
  def paper(filename: String)  = getClass().getResourceAsStream(s"/papers/$filename")
  def paperUrl(filename: String)  = getClass().getResource(s"/papers/$filename")
}
