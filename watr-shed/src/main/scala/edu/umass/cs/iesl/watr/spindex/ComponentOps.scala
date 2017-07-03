package edu.umass.cs.iesl.watr
package spindex

import watrmarks.Label
// import utils.Histogram, Histogram._
import textboxing.{TextBoxing => TB}, TB._
import watrmarks.{StandardLabels => LB}

import utils.SlicingAndDicing._
import utils.{CompassDirection => Compass}
import tracing.VisualTracer, VisualTracer._
import scala.collection.mutable
// import utils.EnrichNumerics._
import TextReflowConversion._
// import TypeTags._

import geometry._

import geometry.syntax._
import utils.ExactFloats._

object ComponentOperations {
  import textreflow.data._
  import utils.EnglishDictionary

  import utils.ScalazTreeImplicits._
  import scalaz._, Scalaz._

  def renderRoleTree(c: Component): TB.Box = {
    c.toRoleTree(LB.VisualLine, LB.TextSpan, LB.PageAtom)
      .map(_.toString())
      .drawBox
  }


  // def vtraceHistogram(hist: Histogram): TraceLog = {
  //   vtraceHistogram(
  //     hist.getFrequencies
  //       .sortBy(_.frequency)
  //       .reverse
  //       .takeWhile(_.frequency > 0)
  //       .map{b=>(b.value, b.frequency)},
  //     hist.getStartingResolution, hist.getComputedResolution
  //   )
  // }

  // def vtraceHistogram(vfs: Seq[(FloatExact, Double)], resStart: Double, resComputed: Double): TraceLog = {
  //   message(
  //     vjoin()(
  //       s"histogram: resolution(in)=${resStart.pp}, resolution(computed):${resComputed.pp}", indent(2)(
  //         vjoin(AlignLeft)(
  //           "Val:",
  //           "Freq:"
  //         ) + hjoins(sep=" ")(
  //           vfs.map({case d =>
  //             vjoin(AlignRight)(
  //               d._1.pp,
  //               d._2.pp
  //             )
  //           })
  //         ))
  //     )
  //   )
  // }

  // def getMostFrequentValuesAndFreqs(vtrace: VisualTracer)(in: Seq[FloatExact], resolution: Double): Seq[(FloatExact, Double)] = {
  //   val hist = histogram(in, resolution)

  //   val res = hist.getFrequencies
  //     .sortBy(_.frequency)
  //     .reverse
  //     .takeWhile(_.frequency > 0)
  //     .map{b=> (b.value, b.frequency)}

  //   // vtrace.trace(vtraceHistogram(hist))
  //   res
  // }


  // def getMostFrequentValues(vtrace: VisualTracer)(in: Seq[FloatExact], resolution: Double): Seq[FloatExact] = {
  //   getMostFrequentValuesAndFreqs(vtrace)(in, resolution).map(_._1)
  // }

  def joinTextLines(line1: TextReflow, line2: TextReflow, force: Boolean=false)(dict: EnglishDictionary): TextReflow = {

    val line1Text = line1.toText()
    val line2Text = line2.toText()

    val lineMinLenReq = line1Text.length > 10  || force // magic # for minimum line length
    val endsWithDash = line1Text.lastOption.exists(_ == '-')
    val isBrokenWord = endsWithDash && lineMinLenReq  // && !hasNonLetterPrefix

    // vtrace.trace("Broken word joined:" withInfo(word))
    // vtrace.trace("Broken word hyphenated:" withInfo(s"${w1}-${w2}"))

    def dehyphenate(): TextReflow = {
      val dehyph = line1.modifyCharAt(line1Text.length-1)({case _ => Some("")})
      val res = join("")(dehyph, line2)
      res
    }
    def concat(): TextReflow = {
      join("")(line1, line2)
    }

    if (isBrokenWord) {
      val wordHalfFirst = line1Text.split(" ").lastOption
      val wordHalfSecond =  line2Text.split(" ").headOption
      (wordHalfFirst, wordHalfSecond) match {
        case (Some(firstHalf), Some(secondHalf)) =>
          val w1 = firstHalf.dropRight(1)
          val w2 = secondHalf.reverse.dropWhile(!_.isLetter).reverse
          // val w2Extra = secondHalf.reverse.takeWhile(!_.isLetter).reverse

          val word = w1 + w2
          if (dict.contains(word)) {
            dehyphenate()
          } else if (dict.contains(w1) && dict.contains(w2)) {
            concat()
          } else {
            dehyphenate()
          }
        case _ =>
          dehyphenate()
      }
    } else {
      join(" ")(line1, line2)
    }
  }


  def centerX(cb: CharAtom) = cb.bbox.toCenterPoint.x
  def centerY(cb: CharAtom) = cb.bbox.toCenterPoint.y

  def spaceWidths(cs: Seq[CharAtom]): Seq[FloatExact] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => (c2.bbox.left - c1.bbox.right)
      case _  => 0.toFloatExact()
    })

    dists :+ 0d.toFloatExact()
  }

  def pairwiseSpaceWidths(cs: Seq[Component]): Seq[FloatExact] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => (c2.bounds.left - c1.bounds.right)
      case _  => 0d.toFloatExact()
    })

    dists :+ 0d.toFloatExact()
  }


  implicit class ComponentOps_RicherComponent(val theComponent: Component) extends AnyVal {
    // TODO move this operation (and other more fundamental operations) into another class
    def ungroupChildren(label: Label)(
      fn: (Component) => Boolean
    ): Unit = {

      val flat = mutable.MutableList[Component]()

      for { child <- theComponent.getChildren(label) } {
        if (fn(child)) {
          val cchilds = child.getChildren(label)
          flat ++= cchilds
          theComponent.mpageIndex.removeComponent(child)
        } else {
          flat += child
        }

        theComponent.setChildren(label, flat)
      }
    }

    def mpageIndex = theComponent.mpageIndex
    def vtrace = theComponent.mpageIndex.vtrace

    def left: FloatExact  = theComponent.bounds.left
    def top: FloatExact  = theComponent.bounds.top
    def height: FloatExact  = theComponent.bounds.height
    def width: FloatExact  = theComponent.bounds.width

    def hasLabel(l: Label): Boolean = theComponent.getLabels.contains(l)

    def hasAnyLabel(ls: Label*): Boolean = {
      ls.exists(theComponent.hasLabel(_))
    }


    def vdist(other: Component): FloatExact = {
      (other.bounds.bottom - theComponent.bounds.bottom)
    }

    def columnContains(other: Component): Boolean = {
      val slopFactor = 4.5d // TODO test this magic number?

      val left = theComponent.bounds.toWesternPoint.x
      val right = theComponent.bounds.toEasternPoint.x

      val otherLeft = other.bounds.toWesternPoint.x + slopFactor
      val otherRight = other.bounds.toEasternPoint.x - slopFactor

      left <= otherLeft && otherRight <= right

    }

    def columnIntersects(other: Component): Boolean = {
      val slopFactor = 0.31d // XXX what is this magic number?

      val otherx0 = other.bounds.toWesternPoint.x-slopFactor
      val otherx1 = other.bounds.toEasternPoint.x+slopFactor
      val candx0 = theComponent.bounds.toWesternPoint.x
      val candx1 = theComponent.bounds.toEasternPoint.x
      val candRightInside = otherx0 <= candx1 && candx1 <= otherx1
      val candLeftOutside = candx0 < otherx0
      val candLeftInside = otherx0 <= candx0 && candx0 <= otherx1
      val candRightOutside = otherx1 < candx1

      val crossesLeft = candRightInside && candLeftOutside
      val crossesRight = candLeftInside && candRightOutside


      crossesLeft || crossesRight
    }

    def isOverlapped(other: Component): Boolean = {
      theComponent.bounds.intersects(other.bounds)
    }

    def isOverlappedVertically(other: Component): Boolean = {
      !(theComponent.isStrictlyAbove(other) || theComponent.isStrictlyBelow(other))
    }

    def isStrictlyAbove(other: Component): Boolean = {
      val y1 = theComponent.bounds.toPoint(Compass.S).y
      val y2 = other.bounds.toPoint(Compass.N).y
      y1 < y2
    }
    def isStrictlyBelow(other: Component): Boolean = {
      val y1 = theComponent.bounds.toPoint(Compass.N).y
      val y2 = other.bounds.toPoint(Compass.S).y
      y1 > y2
    }

    def isStrictlyLeftOf(other: Component): Boolean = {
      val rightEdge = theComponent.bounds.toEasternPoint.x
      val otherLeftEdge = other.bounds.toWesternPoint.x
      rightEdge < otherLeftEdge
    }

    def isStrictlyRightOf(other: Component): Boolean = {
      val leftEdge = theComponent.bounds.toEasternPoint.x
      val otherRightEdge = other.bounds.toWesternPoint.x
      otherRightEdge < leftEdge
    }

    def candidateIsOutsideLineBounds(other: Component): Boolean = {
      theComponent.isStrictlyLeftOf(other) || theComponent.isStrictlyRightOf(other)
    }


    def isBelow(other: Component) = theComponent.bounds.top > other.bounds.top
    def isAbove(other: Component) = theComponent.bounds.top < other.bounds.top

    def hasSameVCenterPoint(tolerance: Double)(other: Component) =
      theComponent.bounds.toCenterPoint.x.eqFuzzy(tolerance)(other.bounds.toCenterPoint.x)

    def hasSameLeftEdge(tolerance: Double)(other: Component) =
      theComponent.bounds.toPoint(Compass.W).x.eqFuzzy(tolerance)(other.bounds.toPoint(Compass.W).x)

    def isEqualWidth(tolerance: Double)(other: Component) =
      theComponent.bounds.width.eqFuzzy(tolerance)(other.bounds.width)


    def atoms = theComponent.queryInside(LB.PageAtom)

    def orderPageAtomsByFrequency(orderf: (Component) => FloatExact): Seq[(FloatExact, Int)] = {
      val countedAtoms: Map[FloatExact, Int] =
        atoms
          .groupBy(orderf(_))
          .mapValues { _.length }

      countedAtoms.toList
        .sortBy(_._2).reverse
    }

    def getPageAtomTopsByFrequence(): Option[FloatExact] = {
      orderPageAtomsByFrequency(_.bounds.top)
        .headOption.map(_._1)
    }

    def findCommonToplines(): Seq[FloatExact] = {
      vtrace.trace(message("findCommonToplines"))
      orderPageAtomsByFrequency(_.bounds.top)
        .map(_._1)
    }

    def findCommonBaselines(): Seq[FloatExact] = {
      vtrace.trace(message("findCommonBaselines"))
      orderPageAtomsByFrequency(_.bounds.bottom)
        .map(_._1)
    }



    // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
    // def determineSpacings(): Seq[Double] = {
    def determineSpacings(): Seq[FloatExact] = {

      val dists = pairwiseSpaceWidths(atoms)
      // val resolution = determineSpacingsHistResolution

      // val hist = Histogram.histogram(dists, resolution)
      val mostFrequentDists = dists.groupBy(x => x)
        .mapValues { _.length }
        .toList
        .sortBy(_._2).reverse


      // vtrace.trace("determine line/char spacings" withTrace vtraceHistogram(hist))

      mostFrequentDists.map(_._1)
    }



    def labelSuperAndSubscripts(): Unit = {
      vtrace.trace(begin("labelSuperAndSubscripts()"))
      // println(s"pageAtoms = ${atoms}")


      // val tops = findCommonToplines()
      val bottoms = findCommonBaselines()
      // val modalTop = tops.head // - 0.01d
      val modalBottom = bottoms.head // + 0.01d
      // val modalCenterY = (modalBottom + modalTop)/2


      // val supSubTol = (modalCenterY-modalTop) * 0.25
      // val subScriptUpperLimit = modalCenterY-supSubTol
      // val superScriptLowerLimit = modalCenterY+supSubTol

      // indicate a set of h-lines inside theComponent.targetRegion
      // def indicateHLine(y: Double): TargetFigure = y.toHLine
      //   .clipTo(theComponent.targetRegion.bbox)
      //   .targetTo(theComponent.targetRegion.page.stable.pageNum)

      // vtrace.trace(
      //   "modal top     " withTrace indicateHLine(modalTop),
      //   "modal bottom  " withTrace indicateHLine(modalBottom),
      //   "modal center Y" withTrace indicateHLine(modalCenterY)
      // )


      theComponent.getChildren(LB.TextSpan).foreach({ textSpan =>

        val supOrSubList = textSpan.atoms.map { atom =>
          // val cctr = atom.bounds.toCenterPoint
          // val cbottom = atom.bounds.bottom
          // val supSubTolerance = theComponent.bounds.height / 20.0

          lazy val atomBottom =  atom.bounds.bottom

          // vtrace.trace(s" sup/sub, atom.bottom=${atomBottom}, modalBottom=${modalBottom} tol:${supSubTolerance.pp}" withInfo atom.bounds.prettyPrint)
          if (atomBottom.eqFuzzy(0.07d)(modalBottom)) {
            LB.CenterScript
          } else if (atomBottom < modalBottom) {
            // println(s" sup ${atom} ")
            // println(s"    atom.bottom=${atomBottom}, modalBottom=${modalBottom} tol:${supSubTolerance.pp}" withInfo atom.bounds.prettyPrint)
            LB.Sup
          } else {
            LB.Sub
          }

        }

        val labelSpans = supOrSubList.groupByPairs({(l1, l2) => l1 == l2 })
          .map({lls => lls.head})

        val supSubRegions = textSpan
          .groupAtomsIf({(atom1, atom2, pairIndex) =>
            val shouldGroup = supOrSubList(pairIndex) == supOrSubList(pairIndex+1)
            // vtrace.trace(message(s"groupIf ${supOrSubList.toList(pairIndex)} == ${supOrSubList.toList(pairIndex+1)}"))
            shouldGroup
          }, {(region, regionIndex, regionCount) =>
            // vtrace.trace(message(s"groupIf (true) r:${region}, i:${regionIndex}"))
            region.addLabel(labelSpans(regionIndex))
          })

        textSpan.setChildren(LB.TextSpan, supSubRegions)

      })

      theComponent.ungroupChildren(LB.TextSpan)(_ => true)
      vtrace.trace("Ending Tree" withInfo renderRoleTree(theComponent))
      vtrace.trace(end("labelSuperAndSubscripts()"))
    }

    def guessWordbreakWhitespaceThreshold(): FloatExact = {
      val charDists = determineSpacings()

      val charWidths = theComponent.atoms.map(_.bounds.width)
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

      vtrace.trace {
        "guessWordbreakWhitespaceThreshold" withInfo
        s"""| Char Dists     = ${charDists.map(_.pp).mkString(", ")}
            | Sane Dists     = ${saneCharDists.map(_.pp).mkString(", ")}
            | Widest Char    = ${widestChar.pp}
            | Threshold      = ${threshold.pp}
            |""".stripMargin.mbox
      }

      threshold
    }

    def groupTokens(): Unit = {
      vtrace.trace(begin("Split On Whitespace"))
      vtrace.trace(message(s"chars: ${theComponent.chars}"))
      // TODO assert theComponent roleLabel structure is TextSpan/TextSpan*/PageAtom

      val splitValue = guessWordbreakWhitespaceThreshold()

      theComponent.addLabel(LB.Tokenized)

      var dbgGrid = Grid.widthAligned(
        (1, AlignLeft),  // join indicator
        (2, AlignLeft),  // char(s)
        (6, AlignRight), // char.left
        (1, AlignLeft),  // space
        (6, AlignRight), // char.right
        (1, AlignLeft),  // space
        (6, AlignRight), // c1 - c2 dist
        (1, AlignLeft),  // space
        (5, AlignRight)  // char.width
      )
      vtrace.ifTrace({
        dbgGrid = dbgGrid.addRow(
          "J",
          "",
          "LEFT||",
          "",
          "RIGHT|",
          "",
          "DIST||",
          "",
          "WIDTH"
        )
        dbgGrid = dbgGrid.addRow(" ", "  ", "      ", " ", "      ", " ", "      ", " ", "     ")
      })


      theComponent.groupAtomsIf({ (c1, c2, pairIndex) =>

        val pairwiseDist = c2.bounds.left - c1.bounds.right
        val willGroup = pairwiseDist < splitValue

        vtrace.ifTrace({
          dbgGrid = dbgGrid.addRow(
            if(willGroup) "_" else "$",
            c1.chars,
            c1.bounds.left.pp,
            "~",
            c1.bounds.right.pp,
            "~",
            pairwiseDist.pp,
            "~",
            c1.bounds.width.pp
          )
        })

        willGroup

      }, { (newRegion, regionIndex, regionCount) =>
        newRegion.addLabel(LB.Token)
        if (regionCount > 1 && regionIndex < regionCount-1) {
          newRegion.addLabel(LB.WhitespaceAfter)
        }

        vtrace.trace { "appending token" withInfo s"${newRegion.chars}, index ${regionIndex} " }

        theComponent.addChild(LB.TextSpan, newRegion)
      })

      vtrace.trace({
        "line groups" withInfo dbgGrid.toBox().transpose()
      })

      vtrace.trace("Tree after split whitespace" withInfo renderRoleTree(theComponent))

      vtrace.trace(end("Split On Whitespace"))
    }

    // HOTSPOT: textbox operations in particular
    def tokenizeLine(): Option[TextReflow] = {
      // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Print
      vtrace.trace("tokenizeLine: " withInfo theComponent.chars.mkString)

      // val __debug = true

      labelSuperAndSubscripts()

      // if (__debug) {
      //   println("after super/subscript")
      //   println(renderRoleTree(theComponent))
      // }

      // Structure is: VisualLine/TextSpan/[TextSpan[sup/sub/ctr]]
      // Group each TextSpan w/sup/sub/ctr-script label into tokens
      theComponent.getChildren(LB.TextSpan)
        .filter(_.hasAnyLabel(LB.CenterScript, LB.Sub, LB.Sup))
        .foreach{ _.groupTokens() }

      // Now figure out how the super/sub/normal text spans should be joined together token-wise

      theComponent.ungroupChildren(LB.TextSpan)({c =>
        c.hasLabel(LB.CenterScript)
      })


      //   println("after ungroup")
      //   println(renderRoleTree(theComponent))

      // TODO don't compute this multiple times
      val splitValue = guessWordbreakWhitespaceThreshold()

      theComponent.groupChildren(withLabel=LB.TextSpan, newLabel=LB.TextSpan)(
        {(c1, c2, pairIndex) =>
          val pairwiseDist = c2.bounds.left - c1.bounds.right

          pairwiseDist < splitValue
        },{(region, regionIndex, regionCount) =>
          if (regionCount > 1 && regionIndex < regionCount) {
            region.addLabel(LB.WhitespaceAfter)
          }
          region.addLabel(LB.Token)
        }
      )

      theComponent.ungroupChildren(LB.TextSpan)({c =>
        c.getChildren(LB.TextSpan).length == 1
      })

      val asReflow = toTextReflow(theComponent)

      // tracing.VisualTracer.visualTraceLevel = tracing.VisualTraceLevel.Off

      asReflow

    }


  //   def determineNormalTextBounds: LTBounds = {
  //     val mfHeights = getMostFrequentValues(vtrace)(theComponent.atoms.map(_.bounds.height), 0.1d)
  //     val mfTops = getMostFrequentValues(vtrace)(theComponent.atoms.map(_.bounds.top), 0.1d)


  //     val mfHeight= mfHeights.headOption.getOrElse(0.toFloatExact)
  //     val mfTop = mfTops.headOption.getOrElse(0.toFloatExact)

  //     theComponent.atoms
  //       .map({ c =>
  //         val cb = c.bounds
  //         LTBounds(
  //           left=cb.left, top=mfTop,
  //           width=cb.width, height=mfHeight
  //         )
  //       })
  //       .foldLeft(theComponent.atoms.head.bounds)( { case (b1, b2) =>
  //         b1 union b2
  //       })
  //   }

  }
}
