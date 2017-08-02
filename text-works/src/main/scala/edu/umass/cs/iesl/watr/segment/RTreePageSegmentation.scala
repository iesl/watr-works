package edu.umass.cs.iesl.watr
package segment


import edu.umass.cs.iesl.watr.corpora.DocumentZoningApi
import edu.umass.cs.iesl.watr.extract.PdfTextExtractor
import spindex._
import utils.SlicingAndDicing._

import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._
import org.dianahep.{histogrammar => HST}

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import shapeless.lens
import PageComponentImplicits._
import edu.umass.cs.iesl.watr.tracing.VisualTracer
import textreflow.data._
import scala.concurrent.duration._

object DocumentSegmenter {

  def createSegmenter(stableId: String@@DocumentID, pdfPath: Path, docStore: DocumentZoningApi): DocumentSegmenter = {
    println(s"extracting ${stableId} chars")
    val pageAtomsAndGeometry = PdfTextExtractor.extractChars(stableId, pdfPath)
    val mpageIndex = new MultiPageIndex(stableId, docStore)

    val pageIdL = lens[CharAtom].pageRegion.page.pageId
    val imgPageIdL = lens[PageItem.ImageAtom].pageRegion.page.pageId
    val pathPageIdL = lens[PageItem.Path].pageRegion.page.pageId

    val docId = docStore.addDocument(stableId)
    pageAtomsAndGeometry.foreach { case(regions, geom)  =>
      val pageId = docStore.addPage(docId, geom.id)
      docStore.setPageGeometry(pageId, geom.bounds)
      mpageIndex.addPage(geom)

      regions.foreach {
        case cb:CharAtom if !cb.isNonPrintable =>
          // modify the pageId to match the one assigned by docStore
          val update = pageIdL.modify(cb){_ => pageId}
          mpageIndex.addCharAtom(update)

        case cb:PageItem.ImageAtom =>
          val update = imgPageIdL.modify(cb){_ => pageId}
          mpageIndex.addImageAtom(update)

        case cb:PageItem.Path =>
          val update = pathPageIdL.modify(cb){_ => pageId}
          mpageIndex.addPathItem(update)

        case cb => println(s"error adding ${cb}")
      }
    }

    new DocumentSegmenter(mpageIndex)
  }

  import com.github.davidmoten.rtree
  import rtree._
  import rtree.{geometry => RG}
  import spindex._
  import rx.functions.Func1

  def createRTreeSerializer(): Serializer[Component, RG.Geometry] = {
    val ccSerializer = new Func1[Component, Array[Byte]]() {
      override def call(entry: Component): Array[Byte] = {
        Component.Serialization.serialize(entry)
      }
    }

    val ccDeSerializer = new Func1[Array[Byte], Component]() {
      override def call(bytes: Array[Byte]): Component = {
        Component.Serialization.deserialize(bytes)
      }
    }

    val ser: Serializer[Component, RG.Geometry] = Serializers
      .flatBuffers[Component, RG.Geometry]()
      .serializer(ccSerializer)
      .deserializer(ccDeSerializer)
      .create()

    ser
  }

}

class DocumentSegmenter(val mpageIndex: MultiPageIndex) {
  import com.sksamuel.scrimage.{X11Colorlist => Clr, Color}

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  val vtrace = new VisualTracer()

  val __debug = true


  val labelColors: Map[Label, Color] = {
    Map(
      (LB.VisualLineModal        , Clr.Cornsilk4),
      (LB.VisualLine             , Clr.Plum),
      (LB.PageAtomTmp            , Clr.DarkBlue),
      (LB.PageAtomGrp            , Clr.YellowGreen),
      (LB.PageAtom               , Clr.Grey80),
      (LB.PathBounds             , Clr.Thistle),
      (LB.LinePath               , Clr.Green),
      (LB.HLinePath              , Clr.Black),
      (LB.VLinePath              , Clr.LimeGreen),
      (LB.Image                  , Clr.DarkCyan),
      (LB.LineByHash             , Clr.Firebrick3),
      (LB.LeftAlignedCharCol     , Clr.Orange4),
      (LB.WhitespaceColCandidate , Clr.Green),
      (LB.WhitespaceCol          , Clr.Peru),
      (LB.ReadingBlock           , Clr.Red),
      (LB.Marked                 , Clr.Red4)
    )
  }


  lazy val pageIdMap: Map[Int@@PageID, Int@@PageNum] =
    docStore.getPages(docId).zipWithIndex.map{
      case (pageId, pageNum) => (pageId, PageNum(pageNum))
    }.toMap


  def createZone(label: Label, pageRegions: Seq[PageRegion]): Option[Int@@ZoneID] = {
    docStore.labelRegions(label, pageRegions)
  }


  def runPageSegmentation(): Unit = {

    val pageRegions = for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } yield {

      println(s"Seg. p.${pagenum} id.${pageId}")
      val pageSegmenter = new PageSegmenter(pageId, PageNum(pagenum))

      pageSegmenter.runLineDeterminationOnPage()

      val pageGeometry = pageSegmenter.pageGeometry

      docStore.getTargetRegion(
        docStore.addTargetRegion(pageId, pageGeometry)
      ).toPageRegion()
    }

    createZone(LB.DocumentPages, pageRegions)
  }

  class PageSegmenter(
    pageId: Int@@PageID,
    pageNum: Int@@PageNum
  ) {
    val pageGeometry = docStore.getPageGeometry(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex

    val segvisRootPath = pwd / s"${stableId}-segs.d"

    val vis = new RTreeVisualizer(pageIndex, labelColors, segvisRootPath, vtrace)

    vis.cleanRTreeImageFiles()


    // def writeRTreeImage(name: String, l0: Label, labels: Label*): Unit = {
    //   vtrace.ifTrace {
    //     if (__debug) {
    //       val image = vis.createRTreeImage(l0, labels:_*)
    //       vis.writeRTreeImage(name, image)
    //     }
    //   }
    // }

    // def rtreeSearchHasAllLabels(
    //   queryRegion: LTBounds,
    //   role: Label, labels: Label*
    // ): Seq[Component] = {

    //   rTreeIndex.search(queryRegion, {cc =>
    //     (cc.roleLabel == role &&
    //       labels.forall(cc.hasLabel(_)))
    //   })
    // }

    def rtreeSearchHasAllLabels(
      queryRegion: LTBounds,
      labels: Label*
    ): Seq[Component] = {

      rTreeIndex.search(queryRegion, {cc =>
        val overlapLR = (
          cc.bounds.left < queryRegion.right
            && cc.bounds.right > queryRegion.left
        )
        val overlapTB = (
          cc.bounds.top < queryRegion.bottom
            && cc.bounds.bottom > queryRegion.top
        )

        val overlapping = overlapLR && overlapTB

        overlapping && labels.forall(cc.hasLabel(_))
      })
    }

    def rtreeSearchHasLabel(
      queryRegion: LTBounds,
      label: Label
    ): Seq[Component] = {

      rTreeIndex.search(queryRegion, {cc =>
        cc.hasLabel(label)
      })
    }


    def rtreeSearch(
      queryRegion: LTBounds,
      label: Label, labels: Label*
    ): Seq[Component] = {
      val lbls = label :: labels.toList

      rTreeIndex.search(queryRegion, {cc =>
        lbls.contains(cc.roleLabel)
      })
    }

    def rtreeSearchOverlapping(
      queryRegion: LTBounds,
      label: Label, labels: Label*
    ): Seq[Component] = {
      val lbls = label :: labels.toList

      rTreeIndex.search(queryRegion, {cc =>
        val overlapLR = (
          cc.bounds.left < queryRegion.right
            && cc.bounds.right > queryRegion.left
        )
        val overlapTB = (
          cc.bounds.top < queryRegion.bottom
            && cc.bounds.bottom > queryRegion.top
        )

        val overlapping = overlapLR && overlapTB

        overlapping && lbls.contains(cc.roleLabel)
      })
    }

    def rtreeSearchLineHasLabel(
      queryRegion: Line,
      label: Label
    ): Seq[Component] = {

      rTreeIndex.searchLine(queryRegion, {cc =>
        cc.hasLabel(label)
      })
    }

    def rtreeSearchLine(
      queryRegion: Line,
      label: Label, labels: Label*
    ): Seq[Component] = {
      val lbls = label :: labels.toList

      rTreeIndex.searchLine(queryRegion, {cc =>
        lbls.contains(cc.roleLabel)
      })
    }

    def labelRegion(bbox: LTBounds, label: Label, text: Option[String]=None): RegionComponent = {
      val regionId = docStore.addTargetRegion(pageId, bbox)
      val pageRegion = docStore.getTargetRegion(regionId).toPageRegion
      mpageIndex.createRegionComponent(pageRegion, label, text)
    }



    def runLineDeterminationOnPage(): Unit = {

      mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
        mpageIndex.labelRegion(Seq(imgCC), LB.Image)
      }

      determineLines()
    }


    def determineLines(): Unit = {
      val components = mpageIndex.getPageAtoms(pageNum)

      def stdLabels(lls: Label*) = List(
        LB.Image, LB.HLinePath, LB.VLinePath, LB.LinePath, LB.WhitespaceCol, LB.ReadingBlock, LB.VisualLineModal
      ) ++ lls.toList


      approximateLineBins(components)

      vis.writeRTreeImage("01-lineHashing", LB.LineByHash, stdLabels():_*)

      splitLinesWithOverlaps()

      vis.writeRTreeImage("02-splitLineHashing", LB.LineByHash, stdLabels():_*)

      findCandidateWhitespaceCols(components)

      vis.writeRTreeImage("03-colCandidates", LB.LineByHash, stdLabels(LB.LeftAlignedCharCol, LB.WhitespaceColCandidate):_*)

      combineCandidateWhitespaceCols()

      vis.writeRTreeImage("04-colsCombined", LB.LineByHash, stdLabels():_*)

      splitLinesOnWhitespaceColumns()

      vis.writeRTreeImage("05-LinesSplitByCols", LB.LineByHash, stdLabels():_*)

      val orderedRegions = findReadingOrder(pageGeometry)


      // vis.writeRTreeImage("06-ReadingOrder", LB.LineByHash, stdLabels():_*)

      rewritePathObjects(orderedRegions)

      findVisualLines(orderedRegions)

      vis.writeRTreeImage("06-VisualLines", LB.VisualLine, stdLabels(LB.PageAtom):_*)

      // reorder visual lines within and across reading blocks

      for {
        regionBounds <- orderedRegions
        vlineRoots = rtreeSearchHasAllLabels(regionBounds, LB.VisualLine, LB.Canonical)
        line <- vlineRoots.sortBy(_.bounds.bottom)
      }  {
        // visualLineClusters
        pageIndex.getComponentText(line, LB.VisualLine)
          .foreach { textRow =>
            val text = textRow.toText()
            println(s"${regionBounds}:   ${text}")
          }
        line
      }

      // pageIndex.addCluster(LB.ReadingOrder, readingOrder)


      // find line-joins
    }


    def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[Component]): FloatExact = {
      // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
      def pairwiseSpaceWidths(): Seq[FloatExact] = {
        val cpairs = sortedLineCCs.sliding(2).toList

        val dists = cpairs.map({
          case Seq(c1, c2)  => (c2.bounds.left - c1.bounds.right)
          case _  => 0d.toFloatExact()
        })

        dists :+ 0d.toFloatExact()
      }

      def determineSpacings(): Seq[FloatExact] = {
        val dists = pairwiseSpaceWidths()

        val mostFrequentDists = dists.groupBy(x => x)
          .mapValues { _.length }
          .toList
          .sortBy(_._2).reverse

        mostFrequentDists.map(_._1)
      }
      val charDists = determineSpacings()

      val charWidths = sortedLineCCs.map(_.bounds.width)
      val widestChar = charWidths.max

      // Don't  accept a space wider than (some magic number)*the widest char?
      val saneCharDists = charDists
        .filter(_ < widestChar*2 )
        .filter(_ == 0)

      def resolution =  0.3d

      // Try to divide the list of char dists into 2 groups, small gap and large gap:
      // See if we can divide our histogram values by some value > 2*histResolution
      val distGroups = saneCharDists.groupByPairs( { (c1, c2) =>
        math.abs((c2 - c1).asDouble()) < resolution
      })


      val threshold = if (saneCharDists.length == 1) {
        // If there is only 1 distance, the line is only 1 word (no word breaks)
        1.0d.toFloatExact()
      } else if (distGroups.length >= 2) {
        // vtrace.trace(message(""))
        val d1 = distGroups(0).last
        val d2 = distGroups(1).head

        (d1+d2) / 2
      } else if (saneCharDists.length >= 2) {
        // Take most common space to be char space within words
        val modalLittleGap = saneCharDists.head
        // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
        val modalBigGap = saneCharDists
          .drop(1)
          .filter(_ > modalLittleGap)
          .headOption.getOrElse(modalLittleGap)

        (modalBigGap+modalLittleGap)/2
      } else {
        // Fallback to using unfiltered char dists
        val modalLittleGap = charDists.head
        // The next most frequent space (that is larger than the within-word space) is assumed to be the space between words:
        val modalBigGap = charDists
          .drop(1)
          .filter(_ > modalLittleGap)
          .headOption.getOrElse(modalLittleGap)

        (modalBigGap*2+modalLittleGap)/3
      }

      threshold
    }


    def insertSpacesInRow(textRow: TextGrid.Row): TextGrid.Row = {
      val lineCCs = textRow.cells.collect{
        case TextGrid.ComponentCell(cc) => cc
      }

      val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

      val maybeGroups = textRow.groupBy { (c1, c2) =>
        val pairwiseDist = c2.bounds.left - c1.bounds.right
        val sameGroup = pairwiseDist < splitValue
        sameGroup
      }


      maybeGroups.map { groupCursor =>

        val finalGroups = groupCursor.unfoldBy { group =>
          if (!group.atEnd) {
            val ins = group.focus.last.cloneCell()
            Some(group.insertRight(ins.copy(text=" ")))
          } else {
            Some(group)
          }
        }

        finalGroups.toRow
      } getOrElse { textRow }

    }

    def deleteComponentsWithLabel(l: Label): Unit = {
      pageIndex.getComponentsWithLabel(l)
        .foreach { cc =>
          mpageIndex.removeComponent(cc)
        }
    }






    def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit = {

      val gifBuilder = vis.gifBuilder("findVisualLines", 500.millis)

      pageIndex.getComponentsWithLabel(LB.PageAtom)
        .foreach{ a => a.addLabel(LB.PageAtomTmp) }

      gifBuilder.addFrame("Starting", LB.PageAtomTmp)

      for {
        textBlock <- orderedTextBlocks
      } {

        gifBuilder.indicate("Examining Block", textBlock, LB.PageAtomTmp)


        val sortedHashedLinesWithChars = (for {
          hashedLineCC <- rtreeSearch(textBlock, LB.LineByHash)
        } yield {
          val charsInRegion = rtreeSearchHasLabel(hashedLineCC.bounds, LB.PageAtomTmp)
          (hashedLineCC, charsInRegion)
        }).sortBy { case (_, chars) => chars.length }.reverse


        sortedHashedLinesWithChars.zipWithIndex
          .foreach{ case ((hashLineCC, hashedChars), index) =>
            val remainingChars = hashedChars.filter(_.hasLabel(LB.PageAtomTmp)).nonEmpty
            if (remainingChars) {

              gifBuilder.indicate(s"Processing Hash-Line ${index} of ${sortedHashedLinesWithChars.length}", hashLineCC.bounds(), LB.PageAtomTmp)

              val extendedLineRegion = hashLineCC.bounds.withinRegion(textBlock)
                .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
                .getOrElse { hashLineCC.bounds }


              // Progressively scan from center out to find super/subs
              val height = extendedLineRegion.height
              val top = extendedLineRegion.top

              val centerLine = extendedLineRegion
                .copy(height = height/2, top=top+height/4)


              gifBuilder.indicate("Search Line Region", centerLine, LB.PageAtomTmp)

              // Now find all chars in queryRegion and string them together into a single visual line
              val visualLineAtoms = rtreeSearchHasLabel(centerLine, LB.PageAtomTmp)

              if (visualLineAtoms.nonEmpty) {
                val xSortedAtoms = visualLineAtoms.sortBy(_.bounds.left).toSeq

                val visualLineBBox = xSortedAtoms.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

                gifBuilder.indicate("Found VLine Atoms", visualLineBBox, LB.PageAtomTmp)

                xSortedAtoms.foreach { cc =>
                  cc.removeLabel(LB.PageAtomTmp)
                  cc.addLabel(LB.PageAtomGrp)
                }

                val visualLineCC = labelRegion(visualLineBBox, LB.VisualLine)

                // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Off
                createTextRowFromVisualLine(visualLineCC, xSortedAtoms)
                // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print

                xSortedAtoms.foreach { cc =>
                  cc.removeLabel(LB.PageAtomGrp)
                }
              }

            }
          }
      }

      gifBuilder.finish()

    }

    def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
      ccs.groupBy{f(_)}.toSeq
        .sortBy({ case (_, atoms) => atoms.length })
        .reverse.headOption.map(_._1)
    }

    def createTextRowFromVisualLine(visualLineCC: Component, visualLineAtoms: Seq[Component]): Unit = {

      val visualLineBounds = visualLineCC.bounds()

      val gifBuilder = vis.gifBuilder(
        s"createTextRowsFromVisualLines-${visualLineBounds.left}-${visualLineBounds.bottom}",
        1.seconds
      )


      gifBuilder.indicate(
        s"Text From VisualLine ${visualLineBounds.left}-${visualLineBounds.bottom}",
        visualLineCC.bounds(), LB.PageAtomGrp, LB.PageAtomTmp
      )


      def shrinkVisualLineToModalTopAndBottom(): LTBounds = {

        val modalBaselineI = modalValue(visualLineAtoms, _.bounds().bottom.unwrap)
          .getOrElse(visualLineCC.bounds.bottom.unwrap)

        val modalBaseline = FloatRep(modalBaselineI)

        val modalToplineI = modalValue(visualLineAtoms, _.bounds.top.unwrap)
          .getOrElse(visualLineCC.bounds.top.unwrap)

        val modalTopline = FloatRep(modalToplineI)

        val height = modalBaseline-modalTopline

        val visualLineModalBounds = if (height > 0) {
          visualLineCC.bounds.copy(
            top=modalTopline, height=modalBaseline-modalTopline
          )
        } else {
          visualLineCC.bounds()
        }

        gifBuilder.indicate("Modal-base/top VisualLine", visualLineModalBounds)

        visualLineModalBounds
      }

      if (visualLineAtoms.nonEmpty) {
        val visualLineModalBounds = shrinkVisualLineToModalTopAndBottom()

        gifBuilder.indicate(s"VisualLine Bounds", visualLineBounds)
        gifBuilder.indicate(s"ModalVisualLine Bounds", visualLineModalBounds)

        val topLine = visualLineModalBounds.toLine(Dir.Top) // .translate(y=0.5)
        val bottomLine =  visualLineModalBounds.toLine(Dir.Bottom) // .translate(y = -0.5)

        val topIntersections = rtreeSearchLineHasLabel(topLine, LB.PageAtomGrp)
        val bottomIntersections = rtreeSearchLineHasLabel(bottomLine, LB.PageAtomGrp)

        val topIntersects = topIntersections.map(_.id)
        val bottomIntersects = bottomIntersections.map(_.id)

        gifBuilder.indicate(s"Top intersection Line", topLine.bounds(), LB.PageAtomGrp)
        gifBuilder.indicate(s"Bottom intersection Line", bottomLine.bounds(), LB.PageAtomGrp)
        gifBuilder.indicate(s"Top Atoms", topIntersections.map(_.bounds()), LB.PageAtomGrp)
        gifBuilder.indicate(s"Bottom Atoms", bottomIntersections.map(_.bounds()), LB.PageAtomGrp)

        val textRow = TextGrid.Row.fromComponents(visualLineAtoms)

        textRow.foreach{ _ match {
          case cell@ TextGrid.ComponentCell(cc) =>
            val intersectsTop = topIntersects.contains(cc.id)
            val intersectsBottom = bottomIntersects.contains(cc.id)

            if (cc.bounds.bottom == visualLineModalBounds.bottom) {
              // Center-text
            } else if (intersectsTop && !intersectsBottom) {
              gifBuilder.indicate(s"SuperScript", cc.bounds(), LB.PageAtomGrp)
              cell.addLabel(LB.Sup)
            } else if (!intersectsTop && intersectsBottom) {
              gifBuilder.indicate(s"SubScript", cc.bounds(), LB.PageAtomGrp)
              cell.addLabel(LB.Sub)
            } else {
              gifBuilder.indicate(s"???Script", cc.bounds(), LB.PageAtomGrp)
            }

          case _ =>

        }}
        // Convert consecutive sup/sub to single label
        val maybeCursor = textRow.groupBy { case (cell1, cell2) =>
          cell1.labels.contains(LB.Sub) && cell2.labels.contains(LB.Sub)
        }
        maybeCursor.foreach { nCursor =>
          nCursor.foreach{ c =>
            val hasSubLabel = c.focus.exists { _.labels.contains(LB.Sub) }
            if (hasSubLabel) {
              c.focus.foreach { fcell =>
                fcell.pins.remove(LB.Sub.U)
              }
              c.addLabel(LB.Sub)
            }
          }
        }

        val spacedRow = insertSpacesInRow(textRow)

        // println(s""" createTextRowsFromVisualLines: ${visualLineCC}  > ${spacedRow.toText} """)
        val visualLineClusterCC = pageIndex.addCluster(LB.VisualLine, visualLineAtoms)

        pageIndex.setComponentText(visualLineClusterCC, LB.VisualLine, spacedRow)

        // val textReflow = convertTextRowToTextReflow(spacedRow)

        createZone(LB.VisualLine, Seq(visualLineCC.pageRegion)).foreach { zoneId =>
          // docStore.setTextReflowForZone(zoneId, textReflow)
        }
      }
      gifBuilder.finish()
    }


    def convertTextRowToTextReflow(textRow: TextGrid.Row): TextReflow = {

      val textReflowAtoms: Seq[TextReflow] = textRow.cells.map{ _ match {
        case cell@ TextGrid.ComponentCell(cc) => cc match {
          case RegionComponent(id, role, pageRegion, maybeText) =>
            None
          case AtomicComponent(id, charAtom, role) =>
            val tr = if (cell.labels.nonEmpty) {
              labeled(cell.labels.toSet, atom(charAtom))
            } else {
              atom(charAtom)
            }
            Some(tr)
        }
        case cell@ TextGrid.InsertCell(text, loc) =>
          Some(insert(text))

      }}.flatten

      flows(textReflowAtoms)
    }



    // First approximation for text line-groups
    def approximateLineBins(charBoxes: Seq[AtomicComponent]): Unit = {
      charBoxes
        .groupBy{ _.bounds.bottom.unwrap }
        .toSeq
        .map { case (bottomY, charBoxes) =>
          charBoxes.sortBy(_.bounds.left)
        }.foreach{ lineBin =>
          mpageIndex.labelRegion(lineBin, LB.LineByHash)
        }
    }


    def findLeftAlignedCharCols(
      components: Seq[AtomicComponent]
    ): Seq[RegionComponent] = {
      import HST._
      val componentLefts = HST.SparselyBin.ing(1.0, {x: AtomicComponent => x.bounds.left.asDouble()} named "char-lefts")

      components.foreach { componentLefts.fill(_) }

      // Construct a horizontal query, looking to boost scores of "runs" of consecutive left-x-value
      val queryBoxes = componentLefts.bins.toList
        .sortBy { case (bin, counting) => counting.entries }
        .reverse.take(10) //  only consider the 10 tallest cols
        .map{ case (bin, counting) =>
          val bw = componentLefts.binWidth

          LTBounds.Doubles(
            left   = bw * bin,
            top    = 0d,
            width  = bw,
            height = pageGeometry.height.asDouble()
          )
        }

      val res: List[Option[RegionComponent]] =
        queryBoxes.flatMap { query =>
          val intersects = rtreeSearch(query, LB.PageAtom)

          val consecutiveLeftAlignedCharCols =
            intersects.sortBy(_.bounds.bottom)
              .groupByPairs((c1, c2) => c1.bounds.bottom == c2.bounds.bottom)
              .map(_.sortBy(_.bounds.left).head)
              .groupByPairs((c1, c2) => c1.bounds.left == c2.bounds.left)
              .filter{ groups => groups.length > 1 }

          consecutiveLeftAlignedCharCols
            .map{ ccs => mpageIndex.labelRegion(ccs, LB.LeftAlignedCharCol).map(_._1) }


        }

      res.flatten
    }


    def findCandidateWhitespaceCols(
      components: Seq[AtomicComponent]
    ): Unit = {

      val cols = findLeftAlignedCharCols(components)

      cols.foreach { colRegion =>
        val colBounds = colRegion.bounds
        val startingRegion = LTBounds(
          left   = colBounds.left-0.1d,
          top    = colBounds.top,
          width  = 0.01.toFloatExact(),
          height = colBounds.height
        )

        growToMaxEmptySpace(startingRegion)
          .foreach{ emptyRegion =>
            val colIsWideEnough = emptyRegion.width > 4.0d

            if (colIsWideEnough) {
              labelRegion(emptyRegion, LB.WhitespaceColCandidate)
            }
          }

      }
    }


    def leftRightContext(
      cc: Component,
      queryRegion: LTBounds,
      l0: Label,
      labels: Label*
    ): Option[(Seq[Component], Seq[Component])] = {

      cc.bounds.withinRegion(queryRegion)
        .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
        .map { horizontalStripeRegion =>
          rtreeSearch(horizontalStripeRegion, l0, labels:_*)
            .sortBy(_.bounds.left)
            .filterNot(_.id == cc.id)
            .span(_.bounds.left < cc.bounds.left)
        }

      // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

    }

    // Try to detect and possibly rewrite text that is represented as path objects
    def rewritePathObjects(orderedTextBlocks: Seq[LTBounds]): Unit = {
      for {
        textBlock <- orderedTextBlocks
        hlineCC <- rtreeSearchOverlapping(textBlock, LB.HLinePath)
      } yield {
        mpageIndex.removeComponent(hlineCC)

        val width = hlineCC.bounds.width

        if (width.asDouble() < 10d) {
          labelRegion(hlineCC.bounds, LB.PageAtom, Some("—"))
        } else {

          leftRightContext(hlineCC, textBlock, LB.PageAtom, LB.HLinePath) match {
            case Some((lefts, rights)) =>
              if (lefts.isEmpty && rights.isEmpty) {
                labelRegion(hlineCC.bounds, LB.HPageDivider)
              } else {
                labelRegion(hlineCC.bounds, LB.HLinePath)
              }

            case None =>
              labelRegion(hlineCC.bounds, LB.HPageDivider)
          }
        }
      }
    }


    def combineCandidateWhitespaceCols(): Unit = {

      var candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)

      while(candidates.nonEmpty) {
        val candidate = candidates.head

        val overlaps = rtreeSearch(candidate.bounds, LB.WhitespaceColCandidate)
          .filterNot { _.id.unwrap == candidate.id.unwrap }


        overlaps.headOption match {
          case Some(overlap) =>
            // "Burst" the overlapping regions into all constituent parts
            val obbox = overlap.bounds
            val cbbox = candidate.bounds

            val (maybeIntersect, burstRegions) = obbox.withinRegion(cbbox).burstAllAdjacent()

            val allRegions = maybeIntersect.map(_ +: burstRegions).getOrElse(burstRegions)

            allRegions.foreach { bbox =>
              // Discard very small columns
              if (bbox.width > 1.0 && bbox.height > 1.0) {
                // micro-shrink the bbox to prevent it from overlapping its neighbors
                val LTBounds(x, y, w, h) = bbox
                val shrunk = LTBounds(
                  x+0.01.toFloatExact,
                  y+0.01.toFloatExact,
                  w-0.02.toFloatExact,
                  h-0.02.toFloatExact
                )
                if (shrunk.area >= 0) {
                  labelRegion(shrunk, LB.WhitespaceColCandidate)
                } else {
                  // println(s"combineCandidateWhitespaceCols: area <= 0 for ${shrunk} was ${bbox}")
                }
              }
            }

            mpageIndex.removeComponent(candidate)
            mpageIndex.removeComponent(overlap)
          case None =>
            mpageIndex.removeComponent(candidate)
            labelRegion(candidate.bounds(), LB.WhitespaceCol)
        }

        candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)
      }
    }



    def growToMaxEmptySpace(startingRegion: LTBounds): Option[LTBounds] = {

      var currWhiteSpace = startingRegion
      val nudgeFactor = 0.05.toFloatExact()

      def search(q:LTBounds) = rtreeSearch(q, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath, LB.HPageDivider)

      currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Left)
        .foreach { regionLeftOf =>
          // println(s"querying left of ${currWhiteSpace}: ${regionLeftOf}")
          val atomsLeftOf = search(regionLeftOf)

          if (atomsLeftOf.nonEmpty) {
            val rightMostCC = atomsLeftOf.maxBy(_.bounds.right)
            for {
              right <- regionLeftOf.splitVertical(rightMostCC.bounds.right+nudgeFactor)._2
            } {
              currWhiteSpace = currWhiteSpace union right
            }
          }
        }

      currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Top)
        .foreach { regionAbove =>
          // println(s"querying above ${currWhiteSpace}: ${regionAbove}")
          val atomsAbove = search(regionAbove)

          if (atomsAbove.nonEmpty) {
            val bottomMostCC = atomsAbove.maxBy(_.bounds.bottom)
            for {
              bottom <- regionAbove.splitHorizontal(bottomMostCC.bounds.bottom+nudgeFactor)._2
            } { currWhiteSpace = currWhiteSpace union bottom }
          } else {
            currWhiteSpace = currWhiteSpace union regionAbove
          }
        }

      currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Bottom)
        .foreach { regionBelow =>
          // println(s"querying below ${currWhiteSpace}: ${regionBelow}")
          val atomsBelow = search(regionBelow)

          if (atomsBelow.nonEmpty) {
            val topmostCC = atomsBelow.minBy(_.bounds.top)
            for {
              top <- regionBelow.splitHorizontal(topmostCC.bounds.top-nudgeFactor)._1
            } {
              currWhiteSpace = currWhiteSpace union top
            }
          } else {
            currWhiteSpace = currWhiteSpace union regionBelow
          }
        }

      Some(currWhiteSpace)

    }

    def splitLinesWithOverlaps(): Unit = {
      for {
        cc <- pageIndex.getComponentsWithLabel(LB.LineByHash)
      } {
        // Split up lines into strictly non-overlapping regions
        val intersects = rtreeSearch(cc.bounds, LB.LineByHash)


        if (intersects.length > 1) {
          val totalBounds = intersects.map(_.bounds).reduce(_ union _)
          val charsInRegion = rtreeSearch(totalBounds, LB.PageAtom)
          // Remove the LineByHash regions
          // iterate over chars left-to-right and group them into non-overlaps and overlaps
          val allChars = charsInRegion.sortBy(_.bounds.left)

          allChars.groupByPairs { (c1, c2) =>
            c1.bounds.bottom == c2.bounds.bottom
          }.foreach{ ccs =>
            mpageIndex.labelRegion(ccs, LB.LineByHash)
          }

          intersects.foreach { cc =>
            mpageIndex.removeComponent(cc)
          }
        }
      }
    }

    def splitLinesOnWhitespaceColumns(): Unit = {

      for {
        colRegion <- pageIndex.getComponentsWithLabel(LB.WhitespaceCol)
        intersectedLine <- rtreeSearch(colRegion.bounds, LB.LineByHash)
      } {

        val charsInRegion = rtreeSearch(intersectedLine.bounds, LB.PageAtom)
        val allChars = charsInRegion.sortBy(_.bounds.left)

        val (leftSplit, rightSplit) = allChars.span(_.bounds.left < colRegion.bounds.left)

        mpageIndex.labelRegion(leftSplit, LB.LineByHash)
        mpageIndex.labelRegion(rightSplit, LB.LineByHash)

        mpageIndex.removeComponent(intersectedLine)
      }

    }

    def findReadingOrder(initRegion: LTBounds): Seq[LTBounds] = {

      val maybeCol = rtreeSearchOverlapping(initRegion, LB.WhitespaceCol)
        .sortBy(cc => (cc.bounds.top, cc.bounds.left))
        .headOption

      // println(s"  findReadingOrder: wscol: ${maybeCol}")
      maybeCol.map{ wsCol =>
        val colBounds = wsCol.bounds.withinRegion(initRegion)
        val upperRegion = colBounds.adjacentRegions(Dir.TopLeft, Dir.Top, Dir.TopRight)
        val lowerRegion = colBounds.adjacentRegions(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)

        val leftRegion = colBounds.adjacentRegion(Dir.Left)
        val rightRegion = colBounds.adjacentRegion(Dir.Right)

        // println(s"  examining: ${ Seq(leftRegion, rightRegion, lowerRegion) }")


        val rs = Seq(leftRegion, rightRegion, lowerRegion)
          .flatten.flatMap{ r =>
            findReadingOrder(r)
          }

        upperRegion.map( _ +: rs ).getOrElse(rs)
      } getOrElse { Seq(initRegion) }
    }


  }
}
