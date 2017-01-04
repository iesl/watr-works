package edu.umass.cs.iesl.watr
package table  //;import acyclic.file

import textreflow._
import geometry._

trait TextReflowEnrichments {

  implicit class RicherTextReflow(val theTextReflow: TextReflow) {

    def showTargetRegions(): Seq[TargetRegion] = {
      theTextReflow.targetRegions()
    }

    def printTree(): TB.Box = {
      prettyPrintTree(theTextReflow)
    }
  }

}
