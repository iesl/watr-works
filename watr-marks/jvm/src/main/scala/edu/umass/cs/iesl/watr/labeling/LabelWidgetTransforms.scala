package edu.umass.cs.iesl.watr
package labeling

// import textreflow.data._
// import geometry._
// import geometry.syntax._
import LabelWidgetF._
import corpora._
// import rindex._
// import watrmarks._

import matryoshka._
import matryoshka.implicits._

object LabelWidgetTransforms {

  def addZoneIndicators(lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {

    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {
        case RegionOverlay(under, overlays) =>
      }

      lw0
    }

    lwidget.transCata[LabelWidget](addIndicator)
  }
}
