package edu.umass.cs.iesl.watr
package watrcolors
package server


class SvgOverviewServer(
  config: PdfCorpusConfig
) extends SvgOverviewApi  {
  val svgRepoPath = config.rootDirectory

  def createView(svgFilename: String): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", new html.SvgOverviewView(config).init(svgFilename).toString)
    )
  }

  import better.files._
  import scala.collection.mutable

  def getCermineOverlay(svgFilename: String): List[BBox] = {
    import pl.edu.icm.cermine.ComponentConfiguration
    import pl.edu.icm.cermine.ExtractionUtils
    import pl.edu.icm.cermine.structure.model.BxBounds
    import scala.collection.JavaConversions._
    import watrmarks.dom
    import edu.umass.cs.iesl.watr.watrmarks.Matrix
    import edu.umass.cs.iesl.watr.watrmarks.dom.Transformable
    import watrmarks.StandardLabels
    import java.io.InputStreamReader

    val conf = new ComponentConfiguration()
    // svgFilename looks like: 101016jcarbon201407065.pdf.d/101016jcarbon201407065.pdf.svg
    //   so the pdf filename would be: 101016jcarbon201407065.pdf
    val pdfFilename = svgFilename.split(".d/")(0)

    val pdf = File(svgRepoPath, pdfFilename)
    val svg = File(svgRepoPath, svgFilename)

    println(s"server loading pdf: ${pdf.path}, svg: ${svg.path}")


    val watrDom = svg.inputStream.map {is =>
      dom.readWatrDom(
        new InputStreamReader(is),
        StandardLabels.bioDict
      )
    }.headOption.getOrElse(
      sys.error(s"problem reading svg ${svg.path}")
    )

    val overlays = mutable.ArrayBuffer[BBox]()
    // var currTop: Double = 0
    // var currBottom: Double = -1

    var currSvgBounds: dom.Rect = dom.Rect(0, 0, 0, 0)
    var curBxBounds: BxBounds = new BxBounds(0, 0, 0, 0)


    pdf.inputStream.map {is =>
      println("starting cermine extractStructure")

      val  structuredDoc = ExtractionUtils.extractStructure(conf, is)
      println("done with cermine extractStructure")

      def formatBounds(bounds: BxBounds): String = {
        val x = bounds.getX
        val y = bounds.getY
        val w = bounds.getWidth
        val h = bounds.getHeight
        s"""($x, $y, w:$w, h:$h)"""
      }

      def boundsToBBox(bounds: BxBounds): BBox = {
        // fix the y-scale ratio
        val bxYScale = curBxBounds.getHeight // + curBxBounds.getY
        val svgy = currSvgBounds.y + (bounds.getY * currSvgBounds.height) / bxYScale
        val x = bounds.getX

        val w = bounds.getWidth
        val h = bounds.getHeight
        val bb = BBox(x, svgy, w, h)

        println(s"  ${formatBounds(bounds)} -> ${bb}")

        val sdf = (bounds.getY * currSvgBounds.height)
        bb
      }


      val pageLabels = watrDom.annotations.filter { l =>
        println(s"dom label: ${l}")
        l.labelName == StandardLabels.PageLabel
      }

      println(s"""svg page labels: ${pageLabels.mkString(", ")}""")

      // Need to align the bounding boxes per page
      val pages = structuredDoc.asPages.toList.zip(pageLabels)

      println(s"number of pages: ${pages.length}")

      pages.foreach{ case (page, plabel) =>
        currSvgBounds = plabel.bboxes.head
        curBxBounds = page.getBounds

        println(s"svg page box = ${currSvgBounds.x} ${currSvgBounds.y} ${currSvgBounds.width} ${currSvgBounds.height}")
        println(s"bx page box  = ${formatBounds(curBxBounds)}")

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
    import watrmarks.dom


    val svg = File(svgRepoPath, svgFilename)

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
