package edu.umass.cs.iesl.watr
package segment


// import ammonite.{ops => fs}, fs._
// import java.io.InputStream
// import java.net.URI
// import watrmarks._
import spindex._
// import GeometricFigure._

// 
// //import TypeTags._
// import scala.collection.JavaConversions._
// import textboxing.{TextBoxing => TB}, TB._
// import EnrichGeometricFigures._
// import ComponentOperations._

// import utils._
// import utils.{CompassDirection => CDir}
// import tracing.VisualTracer._
// import utils.EnrichNumerics._
// import SlicingAndDicing._

// import scala.collection.mutable

import predsynth._

// import utils.{Histogram, AngleFilter, DisjointSets}
// import Histogram._


object DocsegMerging {

  // import textreflow._

  def mergePriorDocseg(mpageIndex: MultiPageIndex, priorDocseg: Docseg.Docseg): MultiPageIndex = {
    // 1. map each mention to one or more target figures within the prior docseg
    // 2. translate the prior target figures into new mpageIndex geometry
    // 3. query the new mpageIndex for spans of text that fall within the range of the target figures
    // 4. construct new Docseg.Mention(...) with updated text/positions, copying existing role/ids
    // priorDocseg.lines

    // 1. map each mention to one or more target figures within the prior docseg
    priorDocseg.mentions.map({mention =>
      // unserialize TextFlow...

    })


    mpageIndex
  }


}
/*
1.5 mmol copper sulphate (CuSO_{4}¿-127;5H_{2}O) and 10 g tri-"

1.5 mmol copper sulphate (CuSO{4}5H{2}O) and 10 g tri-"
 */
