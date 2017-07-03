package edu.umass.cs.iesl.watr
package utils

import org.scalatest._

import utils.ExactFloats._

import Histogram._

class HistogramTest extends FlatSpec with Matchers {



  it should ".." in {
    val values = List(0, 1, 2, 3).map(_.toFloatExact)

    val hist = histogram(values, 0.2)

    val freqs = hist.getFrequencies()
    println(freqs)


  }
}
