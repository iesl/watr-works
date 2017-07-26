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
// import org.dianahep.histogrammar.ascii._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
// import utils.EnrichNumerics._
import images.{ImageManipulation => IM}
import shapeless.lens
import PageComponentImplicits._

object DocumentSegmenter {
  def createSegmenter(stableId: String@@DocumentID, pdfPath: Path, docStore: DocumentZoningApi): DocumentSegmenter = {
    val pageAtomsAndGeometry = PdfTextExtractor.extractChars(stableId, pdfPath)
    val mpageIndex = new MultiPageIndex(stableId, docStore)

    val pageIdL = lens[CharAtom].pageRegion.page.pageId
    val imgPageIdL = lens[PageItem.ImageAtom].pageRegion.page.pageId
    val pathPageIdL = lens[PageItem.Path].pageRegion.page.pageId

    val docId = docStore.addDocument(stableId)
    pageAtomsAndGeometry.foreach { case(regions, geom)  =>
      val pageId = docStore.addPage(docId, geom.id)
      println(s"added page ${pageId}")
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
}

class DocumentSegmenter(val mpageIndex: MultiPageIndex) {

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

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

      println(s"Page ${pagenum} id=${pageId}")
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



    // def cleanRTreeImageFiles(
    //   pageNum: Int@@PageNum,
    //   name: String,
    //   l0: Label,
    //   labels: Label*
    // ): Path = {
    //   val outRelDir = RelPath(new java.io.File(s"${stableId}-segs.d"))
    //   val outDir = fs.pwd / outRelDir
    //   if (s.exists(outDir)) {
    //     fs.rm(outDir)
    //   }
    //   if (!fs.exists(outDir)) {
    //     fs.mkdir(outDir)
    //   }
    //   val outPath = fs.pwd / outRelDir / RelPath(new java.io.File(s"${name}.pg${pageNum}.png"))
    // }

    val __debug = false

    def writeRTreeImage(
      name: String,
      l0: Label,
      labels: Label*
    ): Unit = {
      if (__debug) {

        import com.sksamuel.scrimage
        import scrimage._
        import X11Colorlist._
        val labelColors: Map[Label, Color] = {
          Map(
            (LB.VisualLine              , X11Colorlist.Plum),
            (LB.PathBounds              , X11Colorlist.RoyalBlue),
            (LB.LinePath                , X11Colorlist.Green),
            (LB.HLinePath               , X11Colorlist.Blue),
            (LB.VLinePath               , X11Colorlist.LimeGreen),
            (LB.Image                   , X11Colorlist.DarkCyan),
            (LB.LineByHash             , Firebrick3),
            (LB.LeftAlignedCharCol     , Blue),
            (LB.WhitespaceColCandidate , Green),
            (LB.WhitespaceCol          , X11Colorlist.Peru),
            (LB.ReadingBlock           , X11Colorlist.Red),
            (LB.Marked                 , MediumTurquoise)
          )
        }

        val LTBounds(l, t, w, h) = pageIndex.getPageGeometry.bounds
        val pageBounds = LTBounds(l, t, w+10, h+10)
        val pageCanvas = IM.createCanvas(pageBounds)

        val lbls = l0 :: labels.toList

        val overlays = rTreeIndex.getItems
          .filter { c =>
            lbls.contains(c.roleLabel)
          }
          .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.getPageGeometry, pageBounds, labelColors(c.roleLabel) ) }

        val embossedCanvas = pageCanvas.draw(overlays.flatten.reverse)

        val bytes = embossedCanvas.image.bytes
        val outRelDir = RelPath(new java.io.File(s"${stableId}-segs.d"))
        val outDir = fs.pwd / outRelDir
        if (!fs.exists(outDir)) {
          fs.mkdir(outDir)
        }

        val outPath = fs.pwd / outRelDir / RelPath(new java.io.File(s"${name}.pg${pageNum}.png"))

        if (fs.exists(outPath)) {
          fs.rm(outPath)
        }
        fs.write(outPath, bytes)

      }
    }

    def runLineDeterminationOnPage(): Unit = {
      val atomicComponents = mpageIndex.getPageAtoms(pageNum)

      mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
        mpageIndex.labelRegion(Seq(imgCC), LB.Image)
      }

      determineLines(atomicComponents)
    }


    def determineLines(
      components: Seq[AtomicComponent]
    ): Unit = {
      def stdLabels(lls: Label*) = List(
        LB.Image, LB.HLinePath, LB.VLinePath, LB.LinePath, LB.WhitespaceCol, LB.ReadingBlock
      ) ++ lls.toList

      approximateLineBins(components)

      writeRTreeImage("01-lineHashing", LB.LineByHash, stdLabels():_*)

      splitLinesWithOverlaps()

      writeRTreeImage("02-splitLineHashing", LB.LineByHash, stdLabels():_*)

      findCandidateWhitespaceCols(components)

      writeRTreeImage("03-colCandidates", LB.LineByHash, stdLabels(LB.LeftAlignedCharCol, LB.WhitespaceColCandidate):_*)

      combineCandidateWhitespaceCols()

      writeRTreeImage("04-colsCombined", LB.LineByHash, stdLabels():_*)

      splitLinesOnWhitespaceColumns()

      writeRTreeImage("05-LinesSplitByCols", LB.LineByHash, stdLabels():_*)

      val orderedRegions = findReadingOrder(pageGeometry)

      orderedRegions.foreach { r => println(s"block> ${r}") }

      findVisualLines(orderedRegions)

      writeRTreeImage("06-VisualLines", LB.VisualLine, stdLabels():_*)

      // find line-joins
      // rewrite line-paths representing symbols into chars (eg, -, =, vinculum)
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

        val finalRow = finalGroups.toRow
        // val hb = lineCCs.head.bounds
        // println(s"${hb}>    ${finalRow.toText}")
        finalRow
      } getOrElse { textRow }

    }

    def findVisualLines(orderedTextBlocks: Seq[LTBounds]): Unit = {

      for {
        textBlock <- orderedTextBlocks
      } {

        val sortedHashedLinesWithChars = (for {
          hashedLineCC <- rtreeSearch(textBlock, LB.LineByHash)
        } yield {
          val charsInRegion = rtreeSearch(hashedLineCC.bounds, LB.PageAtom)
          (hashedLineCC, charsInRegion)
        }).sortBy { case (_, chars) => chars.length }.reverse

        sortedHashedLinesWithChars
          .foreach{ case (hashLineCC, hashedChars) =>
            val extendedLineRegion = hashLineCC.bounds.withinRegion(textBlock)
              .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
              .getOrElse { hashLineCC.bounds }

            // Now find all chars in queryRegion and string them together into a single visual line
            val visualLineAtoms = rtreeSearch(extendedLineRegion, LB.PageAtom)
            val hlineAtoms = rtreeSearch(extendedLineRegion, LB.HLinePath)
            val dashLines = hlineAtoms.map{ hlineCC =>
              val width = hlineCC.bounds.width
              if (width.asDouble() < 10d) {
                mpageIndex.removeComponent(hlineCC)
                Option(labelRegion(hlineCC.bounds, LB.PageAtom, Some("—")))
              } else None
            } flatten


            val comps = (visualLineAtoms ++ dashLines).sortBy(_.bounds.left)
            val compBbox =  comps.map(_.bounds).reduce { (c1, c2) => c1 union c2 }

            // val visualLineCC =
            labelRegion(compBbox, LB.VisualLine)

            // comps.foreach { c=> labelRegion(compBbox, LB.Marked) }

          }

        createTextRowsFromVisualLines(textBlock)
      }
    }

    def createTextRowsFromVisualLines(textBlock: LTBounds): Unit = {
      import textreflow.data._
      for {
        visualLineCC <- rtreeSearch(textBlock, LB.VisualLine).sortBy(_.bounds.bottom)
      } {
        val visualLineBounds = visualLineCC.bounds()

        val visualLineAtoms = rtreeSearch(visualLineBounds, LB.PageAtom)
          .sortBy(_.bounds.left)

        val textRow = TextGrid.Row.fromComponents(visualLineAtoms)

        val topLine = visualLineBounds.toLine(Dir.Top).translate(y=0.5)
        val bottomLine = visualLineBounds.toLine(Dir.Bottom).translate(y = -0.5)

        val topIntersects = rtreeSearchLine(topLine, LB.PageAtom).map(_.id)
        val bottomIntersects = rtreeSearchLine(bottomLine, LB.PageAtom).map(_.id)

        textRow.foreach{ _ match {
          case cell@ TextGrid.ComponentCell(cc) =>
            val intersectsTop = topIntersects.contains(cc.id)
            val intersectsBottom = bottomIntersects.contains(cc.id)

            if (intersectsTop && !intersectsBottom) {
              cell.addLabel(LB.Sup)
            } else if (!intersectsTop && intersectsBottom) {
              cell.addLabel(LB.Sub)
            }

          case _ =>

        }}
        // rewritePathObjects(pageId)
        val spacedRow = insertSpacesInRow(textRow)
        val textReflowAtoms: Seq[TextReflow] = spacedRow.cells.map{ _ match {
          case cell@ TextGrid.ComponentCell(cc) => cc match {
            case RegionComponent(id, role, pageRegion, maybeText) =>
              None
            case AtomicComponent(id, charAtom) =>
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

        val textReflow = flows(textReflowAtoms)
        println(s"reflow>>>${textReflow.toFormattedText()}<<<")

        createZone(LB.VisualLine, Seq(visualLineCC.targetRegion)).foreach { zoneId =>
          docStore.setTextReflowForZone(zoneId, textReflow)
        }
      }
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

      val pageGeometry = docStore.getPageGeometry(pageId)

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
      label: Label
    ): Option[(Seq[Component], Seq[Component])] = {
      cc.bounds.withinRegion(queryRegion)
        .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
        .map { horizontalStripeRegion =>
          rtreeSearch(horizontalStripeRegion, label)
            .sortBy(_.bounds.left)
            .filterNot(_.id == cc.id)
            .span(_.bounds.left < cc.bounds.left)
        }

      // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

    }
    // Try to detect and possibly rewrite text that is represented as path objects
    def rewritePathObjects(): Unit = {

      var candidates = pageIndex.getComponentsWithLabel(LB.HLinePath)

      while(candidates.nonEmpty) {
        val candidate = candidates.head
        // val candidate = rTreeIndex.get(nextCandidate.unwrap).get
        leftRightContext(candidate, pageGeometry, LB.PageAtom) match {
          case Some((lefts, rights)) =>
            lefts.reverse.headOption

          case None =>
            mpageIndex.removeComponent(candidate)
        }

        // does it have an identical path directly above or below, e.g. '=' or triple-equal?
        // is it sitting in the middle of other text to left/right

        candidates = pageIndex.getComponentsWithLabel(LB.HLinePath)
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

      currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Left)
        .foreach { regionLeftOf =>
          // println(s"querying left of ${currWhiteSpace}: ${regionLeftOf}")
          val atomsLeftOf = rtreeSearch(regionLeftOf, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath)

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
          val atomsAbove = rtreeSearch(regionAbove, LB.PageAtom, LB.Image, LB.HLinePath, LB.VLinePath)

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
          val atomsBelow = rtreeSearch(regionBelow, LB.PageAtom, LB.Image, LB.HLinePath, LB.VLinePath)

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

      println(s"  findReadingOrder: wscol: ${maybeCol}")
      maybeCol.map{ wsCol =>
        val colBounds = wsCol.bounds.withinRegion(initRegion)
        val upperRegion = colBounds.adjacentRegions(Dir.TopLeft, Dir.Top, Dir.TopRight)
        val lowerRegion = colBounds.adjacentRegions(Dir.BottomLeft, Dir.Bottom, Dir.BottomRight)

        val leftRegion = colBounds.adjacentRegion(Dir.Left)
        val rightRegion = colBounds.adjacentRegion(Dir.Right)

        println(s"  examining: ${ Seq(leftRegion, rightRegion, lowerRegion) }")


        val rs = Seq(leftRegion, rightRegion, lowerRegion)
          .flatten.flatMap{ r =>
            findReadingOrder(r)
          }

        upperRegion.map( _ +: rs ).getOrElse(rs)
      } getOrElse { Seq(initRegion) }
    }


  }
}
