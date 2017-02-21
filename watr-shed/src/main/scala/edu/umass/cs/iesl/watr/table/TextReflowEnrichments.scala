package edu.umass.cs.iesl.watr
package table

import textreflow.data._

import textboxing.{TextBoxing => TB}

trait TextReflowEnrichments {

  implicit class RicherTextReflow(val theTextReflow: TextReflow) {

    def printTree(): TB.Box = {
      prettyPrintTree(theTextReflow)
    }
  }

}
