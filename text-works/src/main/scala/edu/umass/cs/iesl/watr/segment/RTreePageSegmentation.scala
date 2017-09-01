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

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import shapeless.lens
import PageComponentImplicits._
import edu.umass.cs.iesl.watr.tracing.VisualTracer
import edu.umass.cs.iesl.watr.tracing.TraceCallbacks
import textgrid._
import com.sksamuel.scrimage.{X11Colorlist => Clr, Color}

import org.dianahep.{histogrammar => HST}

object DocumentSegmenter {
  import spindex._

  val noopVisualTracer = new VisualTracer {
    def traceCallbacks: TraceCallbacks = new TraceCallbacks {

    }

  }


  def createSegmenter(
    stableId: String@@DocumentID,
    pdfPath: Path,
    docStore: DocumentZoningApi,
    tracer: VisualTracer = noopVisualTracer
  ): DocumentSegmenter = {
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

    new DocumentSegmenter(mpageIndex, tracer)
  }

  val DebugLabelColors: Map[Label, Color] = {
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

  // val vtrace = new VisualTracer()
}

class DocumentSegmenter(
  val mpageIndex: MultiPageIndex,
  tracer: VisualTracer
) { documentSegmenter =>

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

      println(s"Seg. p.${pagenum} id.${pageId}")
      val pageSegmenter = new PageSegmenter(pageId, PageNum(pagenum), mpageIndex, tracer)

      pageSegmenter.runLineDeterminationOnPage()

      val pageGeometry = pageSegmenter.pageGeometry


      docStore.getTargetRegion(
        docStore.addTargetRegion(pageId, pageGeometry)
      )
    }

    val _ = createZone(LB.DocumentPages, pageRegions)

  }


  def classifyLinePairs(): Unit = {
    for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } {
      val pageIndex = mpageIndex.getPageIndex(PageNum(pagenum))
      for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
        linePair <- lineCCs.sliding(2)
      } {
      }
    }
  }

}


object PageSegmenter {

  def getVisualLinesInReadingOrder(pageIndex: PageIndex): Seq[(Component, Seq[Component])] = {
    val linesPerBlock0 = for {
      block <- pageIndex.getOrdering(LB.ReadingBlocks)
    } yield {
      for {
        visualLineRootCCs <- pageIndex.getRelations(block, LB.HasVisualLines).toList
        visualLineRootCC <- visualLineRootCCs
        lineMembers   <- pageIndex.getClusterMembers(LB.ReadingBlockLines, visualLineRootCC)
      } yield {
        (block, lineMembers)
      }
    }

    val linesPerBlock = linesPerBlock0.flatten

    linesPerBlock
  }

}

class PageSegmenter(
  pageId: Int@@PageID,
  pageNum: Int@@PageNum,
  mpageIndex: MultiPageIndex,
  tracer: VisualTracer
) {

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  val pageGeometry = docStore.getPageGeometry(pageId)
  val pageIndex = mpageIndex.getPageIndex(pageNum)

  val segvisRootPath = pwd / s"${stableId}-segs.d"
  val vis = new RTreeVisualizer(pageIndex, DocumentSegmenter.DebugLabelColors, segvisRootPath, tracer)

  // def tracer() = PageIndex.tracer()

  vis.cleanRTreeImageFiles()


  def labelRegion(bbox: LTBounds, label: Label, text: Option[String] = None): RegionComponent = {
    val regionId = docStore.addTargetRegion(pageId, bbox)
    val pageRegion = docStore.getTargetRegion(regionId)
    mpageIndex.createRegionComponent(pageRegion, label, text)
  }

  def runLineDeterminationOnPage(): Unit = {
    tracer.enter()

    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }

    val lineFinder = new LineFinder(mpageIndex, pageId, pageNum, tracer)
    lineFinder.determineLines()

    val lineClassifier = new LineGroupClassifier(mpageIndex, pageId, pageNum, tracer)
    lineClassifier.classifyLines()

    setPageText()

    tracer.exit()
  }

  def setPageText(): Unit = {
    for {
      pageNum      <- mpageIndex.getPages
      pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    }  {
      val textLines = for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
        (line, n)    <- lineCCs.zipWithIndex
        textRow      <- pageIndex.getComponentText(line, LB.VisualLine).toList
      } yield textRow

      val pageTextGrid = TextGrid.fromRows(textLines)

      docStore.setPageText(pageId, pageTextGrid)
    }
  }





  def deleteComponentsWithLabel(l: Label): Unit = {
    pageIndex.getComponentsWithLabel(l)
      .foreach { cc =>
        pageIndex.removeComponent(cc)
      }
  }


  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
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
        val intersects = pageIndex.rtreeSearch(query, LB.PageAtom)

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


  def findCandidateWhitespaceCols(components: Seq[AtomicComponent]): Unit = {

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
        pageIndex.rtreeSearch(horizontalStripeRegion, l0, labels:_*)
          .sortBy(_.bounds.left)
          .filterNot(_.id == cc.id)
          .span(_.bounds.left < cc.bounds.left)
      }

    // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

  }

  // Try to detect and possibly rewrite text that is represented as path objects
  def rewritePathObjects(orderedTextBlocks: Seq[LTBounds]): Unit = {
    val _ = for {
      textBlock <- orderedTextBlocks
      hlineCC <- pageIndex.rtreeSearchOverlapping(textBlock, LB.HLinePath)
    } yield {
      pageIndex.removeComponent(hlineCC)

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

      val overlaps = pageIndex.rtreeSearch(candidate.bounds, LB.WhitespaceColCandidate)
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

          pageIndex.removeComponent(candidate)
          pageIndex.removeComponent(overlap)
        case None =>
          pageIndex.removeComponent(candidate)
          labelRegion(candidate.bounds(), LB.WhitespaceCol)
      }

      candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)
    }
  }



  def growToMaxEmptySpace(startingRegion: LTBounds): Option[LTBounds] = {

    var currWhiteSpace = startingRegion
    val nudgeFactor = 0.05.toFloatExact()

    def search(q:LTBounds) = pageIndex.rtreeSearch(q, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath, LB.HPageDivider)

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



}
