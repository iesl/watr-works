package edu.umass.cs.iesl.watr
package table

import textreflow.data._
import geometry._

import textboxing.{TextBoxing => TB}

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
