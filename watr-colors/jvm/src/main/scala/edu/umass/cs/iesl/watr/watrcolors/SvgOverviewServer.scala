package edu.umass.cs.iesl
package watr
package watrcolors


object SvgOverviewServer extends SvgOverviewApi  {

  def createView(svgFilename: String): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", html.SvgOverviewPane.init(svgFilename).toString)
    )
  }

  import better.files._
  import scala.collection.mutable

  def getCermineOverlay(svgFilename: String): List[BBox] = {
    import pl.edu.icm.cermine.ComponentConfiguration
    import pl.edu.icm.cermine.ExtractionUtils
    import pl.edu.icm.cermine.structure.model.BxBounds
    import scala.collection.JavaConversions._

    val conf = new ComponentConfiguration()
    val pdfFilename = svgFilename.dropRight(4)

    val svg = File("../svg-repo", pdfFilename)
    println(s"server loading file ${svg.path}")
    val overlays = mutable.ArrayBuffer[BBox]()
    var currTop: Double = 0
    var currBottom: Double = -1

    svg.inputStream.map {is =>

      val  structuredDoc = ExtractionUtils.extractStructure(conf, is)

      def formatBounds(bounds: BxBounds): String = {
        val x = bounds.getX
        val y = bounds.getY
        val w = bounds.getWidth
        val h = bounds.getHeight
        s"""($x, $y, w:$w, h:$h)"""
      }

      def boundsToBBox(bounds: BxBounds): BBox = {
        val x = bounds.getX
        val y = currTop + bounds.getY
        val w = bounds.getWidth
        val h = bounds.getHeight
        BBox(x, y, w, h)
      }


      val pages = structuredDoc.asPages().toList
      pages.foreach{ page =>
        val pbounds = page.getBounds
        currTop = if (currBottom >= 0) currBottom else 0
        currBottom = currTop + pbounds.getHeight + pbounds.getY
        println(s"page: ${formatBounds(page.getBounds)}")
        page.iterator().foreach{ zone =>
          overlays.append(boundsToBBox(zone.getBounds))
          zone.iterator().toList.foreach{ line =>
            // println(s"line: ${line.toText()} ${formatBounds(line.getBounds)} ")
            line.iterator().toList.foreach{ word =>
              word.iterator().toList.foreach{ chunk =>
                // println(s"chunk: ${chunk.toText()} ${formatBounds(chunk.getBounds)} ")
              }
            }
          }
        }
      }
    }
    overlays.toList
  }


  def getCharLevelOverlay(svgFilename: String): List[BBox] = {


    import edu.umass.cs.iesl.watr.watrmarks.Matrix
    import edu.umass.cs.iesl.watr.watrmarks.dom.Transformable
    import watrmarks.StandardLabels
    import java.io.InputStreamReader
    import watr.watrmarks.dom

    val svg = File("../svg-repo", svgFilename)
    println(s"server loading file ${svg.path}")

    val overlays = mutable.ArrayBuffer[BBox]()

    svg.inputStream.map {is =>
      val doc = time("read watr dom") {
        dom.readWatrDom(
          new InputStreamReader(is),
          StandardLabels.bioDict
        )
      }

      val tspans = time("drop/take") {
        doc.toDomCursor.unfoldTSpansCursors.drop(200).take(250)
      }

      tspans.foreach({ domCursor =>
        // get all transforms leading to this tspan

        val transforms = time("get transforms"){domCursor.loc
          .path.reverse.toList
          .flatMap{ _ match {
            case t: Transformable => t.transforms
            case _  => List()
          }}}

        val tspan = domCursor.getLabel.asInstanceOf[dom.TSpan]

        val mFinal = time("matrix mult"){ transforms.foldLeft(Matrix.idMatrix)({
          case (acc, e) =>
            acc.multiply(e.toMatrix)
        })}


        time("all bbox computations") {
          tspan.textXYOffsets.foreach { offsets =>
            time("1 bbox") {
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
          }
        }
      })
    }
    overlays.toList
  }

}
