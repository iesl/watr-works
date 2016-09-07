package edu.umass.cs.iesl.watr
package format

import spindex._
import ComponentOperations._
import EnrichGeometricFigures._
import watrmarks.{StandardLabels => LB}


object RichTextIO {

  import utils.SlicingAndDicing._
  import ComponentRendering.VisualLine
  import textboxing.{TextBoxing => TB}
  import TB._


  def serializeDocumentAsText(zoneIndexer: ZoneIndexer, artifactPath: Option[String]): String = {

    val lineSpine = zoneIndexer.bioSpine("TextBlockSpine")

    val lines = for {
      linec <- lineSpine
      line = linec.component
      rline <- VisualLine.render(line)
    } yield { rline }

    val joinedLines =  vcat(lines)


    vjoin()(
      artifactPath.map(_.box).getOrElse("corpus:unknown artifact".box),
      joinedLines
    ).toString()


  }
}
