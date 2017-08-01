package edu.umass.cs.iesl.watr
package rindex

import org.scalatest._

import geometry._
// import geometry.syntax._

class RTreeIndexTests extends FlatSpec {

  behavior of "RTrees"
  implicit object GeometryIndexable extends RTreeIndexable[GeometricFigure] {
    def id(t: GeometricFigure): Int = 0
    def ltBounds(t: GeometricFigure): LTBounds = ???
  }

  it should "just work" in {
    val rTree = RTreeIndex.createFor[GeometricFigure]()



  }
}
