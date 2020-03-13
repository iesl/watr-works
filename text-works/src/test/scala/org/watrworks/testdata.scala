package edu.umass.cs.iesl.watr


object papers {
  def paper(filename: String)  = getClass().getResourceAsStream(s"/papers/$filename")
  def paperUrl(filename: String)  = getClass().getResource(s"/papers/$filename")
}
