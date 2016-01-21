package edu.umass.cs.iesl.watr
package ext

import watrmarks._
import watrmarks.dom._
import java.io.StringReader


// import com.itextpdf.text.Rectangle
// import java.io.InputStreamReader
// import java.io.FileInputStream
import org.scalatest._
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
// import pl.edu.icm.cermine.content.model.ContentStructure
// import pl.edu.icm.cermine.content.transformers.BxContentStructToDocContentStructConverter
// import pl.edu.icm.cermine.exception.TransformationException
import pl.edu.icm.cermine.structure.model.BxBounds
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxZone
import pl.edu.icm.cermine.structure.model.BxLine
import pl.edu.icm.cermine.structure.model.BxWord
import pl.edu.icm.cermine.structure.model.BxDocument
import pl.edu.icm.cermine.structure.model.BxPage
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import scala.collection.JavaConversions._
import java.io.InputStreamReader
import StandardLabels._


// import spatialindex.libspatialindex._


class SpatialAlignmentTest extends FlatSpec {

  behavior of "spatial alignment"

  it should "create rtree index over cermine-generated content" in {

    val conf = new ComponentConfiguration()

    val  structuredDoc = ExtractionUtils.extractStructure(conf, papers.`6376.pdf`)

    val contentStructure  = ExtractionUtils.extractText(conf, papers.`6376.pdf`)

    def formatBounds(bounds: BxBounds): String = {
      val x = bounds.getX
      val y = bounds.getY
      val w = bounds.getWidth
      val h = bounds.getHeight
       s"""($x, $y, w:$w, h:$h)"""
    }

    val pages = structuredDoc.asPages().toList
    pages.foreach{ page =>
      println(s"page: ${formatBounds(page.getBounds)}")
      page.iterator().foreach{ zone =>
        zone.iterator().toList.foreach{ line =>
          // println(s"line: ${line.toText()} ${formatBounds(line.getBounds)} ")
          line.iterator().toList.foreach{ word =>
            word.iterator().toList.foreach{ chunk =>
              println(s"chunk: ${chunk.toText()} ${formatBounds(chunk.getBounds)} ")
            }
          }
        }
      }
    }

    val doc = readWatrDom(
      new InputStreamReader(papers.`6376.svg`),
      bioDict
    )

    var occursor = doc.toCursor(CharLabel)
    // val charBuffer = mutable.ArrayBuffer[Either[(Char, Char), Char]]()

    while (occursor.isDefined) {
      val ccursor = occursor.get
      val bounds = ccursor.focusedBounds()
      val bbox = bounds.map(_.toString()).mkString("[",", ", "]")
      println(s"${ccursor.toText}: ${bbox}")
      occursor = ccursor.next
    }

  }


}
