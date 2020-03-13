package edu.umass.cs.iesl.watr


object papers {
  // def `6376.pdf` = getClass().getResourceAsStream("/papers/6376.pdf")
  // def `bongard2005.pdf` = getClass().getResourceAsStream("/papers/bongard2005.pdf")
  def paper(filename: String)  = getClass().getResourceAsStream(s"/papers/$filename")
  def paperUrl(filename: String)  = getClass().getResource(s"/papers/$filename")
}
