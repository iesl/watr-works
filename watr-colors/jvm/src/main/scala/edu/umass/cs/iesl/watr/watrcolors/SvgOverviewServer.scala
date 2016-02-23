package edu.umass.cs.iesl
package watr
package watrcolors


object SvgOverviewServer extends SvgOverviewApi  {

  def createView(svgFilename: String): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", html.SvgOverviewPane.init(svgFilename).toString)
    )
  }

  def getCharLevelOverlay(svgFilename: String): List[BBox] = {

    import better.files._
    import edu.umass.cs.iesl.watr.watrmarks.Matrix
    import edu.umass.cs.iesl.watr.watrmarks.dom.Transformable
    import watrmarks.StandardLabels
    import java.io.InputStreamReader
    import watr.watrmarks.dom
    import scala.collection.mutable

    val svg = File("../svg-repo", svgFilename)
    println(s"server loading file ${svg.path}")

    val overlays = mutable.ArrayBuffer[BBox]()

    svg.inputStream.map {is =>
      val doc = dom.readWatrDom(
        new InputStreamReader(is),
        StandardLabels.bioDict
      )

      doc.toDomCursor.unfoldTSpansCursors.take(12).foreach({ domCursor =>
        domCursor.loc.path
        // get all transforms leading to this tspan
        val transforms = domCursor.loc
          .path.reverse.toList
          .flatMap{ _ match {
            case t: Transformable => t.transforms
            case _  => List()
          }}
        val tspan = domCursor.getLabel.asInstanceOf[dom.TSpan]

        val mFinal = transforms.foldLeft(Matrix.idMatrix)({
          case (acc, e) =>
            acc.multiply(e.toMatrix)
        })


        tspan.textXYOffsets.foreach { offsets =>
          val y = offsets.ys(0)
          val ff = tspan.fontFamily
          (offsets.xs :+ offsets.endX).sliding(2).foreach { case List(x, x1) =>
            val tvec = mFinal.transform(watrmarks.Vector(x, y))
            val tvec2 = mFinal.transform(watrmarks.Vector(x1, y))
            val bbox = BBox(
              x = tvec.x,
              y = tvec.y,
              width = tvec2.x - tvec.x,
              height = -5.0
              // info = Some(CharInfo(ff))
            )
            overlays.append(bbox)
          }
        }
      })
    }
    overlays.toList
  }

}
