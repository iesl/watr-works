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
import textgrid._
import com.sksamuel.scrimage.{X11Colorlist => Clr, Color}

object DocumentSegmenter {
  import com.github.davidmoten.rtree
  import rtree._
  import rtree.{geometry => RG}
  import spindex._
  import rx.functions.Func1



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

  val vtrace = new VisualTracer()
}

class DocumentSegmenter(val mpageIndex: MultiPageIndex) { documentSegmenter =>
  // import DocumentSegmenter._

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))


  val __debug = true




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
      val pageSegmenter = new PageSegmenter(pageId, PageNum(pagenum), mpageIndex)

      pageSegmenter.runLineDeterminationOnPage()

      val pageGeometry = pageSegmenter.pageGeometry

      docStore.getTargetRegion(
        docStore.addTargetRegion(pageId, pageGeometry)
      )
    }

    createZone(LB.DocumentPages, pageRegions)
  }

}

class PageSegmenter(
  pageId: Int@@PageID,
  pageNum: Int@@PageNum,
  mpageIndex: MultiPageIndex
) { pageSegmenter =>

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  val pageGeometry = docStore.getPageGeometry(pageId)
  val pageIndex = mpageIndex.getPageIndex(pageNum)
  val rTreeIndex = pageIndex.componentIndex

  val segvisRootPath = pwd / s"${stableId}-segs.d"

  val vis = new RTreeVisualizer(pageIndex, DocumentSegmenter.labelColors, segvisRootPath, DocumentSegmenter.vtrace)

  vis.cleanRTreeImageFiles()


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
    val pageRegion = docStore.getTargetRegion(regionId)
    mpageIndex.createRegionComponent(pageRegion, label, text)
  }



  def runLineDeterminationOnPage(): Unit = {

    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }

    val lineFinder = new LineFinder(mpageIndex, pageId, pageNum)
    lineFinder.determineLines()
  }




  def guessWordbreakWhitespaceThreshold(sortedLineCCs: Seq[PageItem]): FloatExact = {
    // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
    def pairwiseSpaceWidths(): Seq[FloatExact] = {
      val cpairs = sortedLineCCs.sliding(2).toList

      val dists = cpairs.map({
        case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
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

    val charWidths = sortedLineCCs.map(_.bbox.width)
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
      case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
        headItem
    }

    val splitValue = guessWordbreakWhitespaceThreshold(lineCCs)

    val maybeGroups = textRow.groupBy { (c1, c2) =>
      val pairwiseDist = c2.pageRegion.bbox.left - c1.pageRegion.bbox.right
      val sameGroup = pairwiseDist < splitValue
      sameGroup
    }


    maybeGroups.map { groupCursor =>

      val finalGroups = groupCursor.unfoldBy { group =>
        if (!group.atEnd) {
          val ins = group.focus.last.cloneCell()
          Some(group.insertRight(ins.copy(char=' ')))
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
