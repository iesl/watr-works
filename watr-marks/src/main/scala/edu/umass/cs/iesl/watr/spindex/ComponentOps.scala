package edu.umass.cs.iesl.watr
package spindex


import watrmarks.{StandardLabels => LB, Label}
import utils.Histogram, Histogram._
import textboxing.{TextBoxing => TB}, TB._

import EnrichGeometricFigures._
import utils.SlicingAndDicing._
import utils.{CompassDirection => Compass}
import utils.VisualTracer, VisualTracer._
import spindex.GeometricFigure._
import scala.collection.mutable
// import ComponentRendering.PageAtom
import utils.EnrichNumerics._
import ComponentRendering.{VisualLine=>CVisualLine}



object ComponentOperations {
  import textflow.TextReflow._
  import textflow.TextReflowRendering._
  import textflow.TextReflowOps._
  import utils.EnglishDictionary

  def joinTextLines(line1: TextReflow, line2: TextReflow)(dict: EnglishDictionary): TextReflow = {

    val line1Text = line1.toText()
    val line2Text = line2.toText()

    val lineMinLenReq = line1Text.length > 10 // magic # for minimum line length
    // val hasNonLetterPrefix = line1.chars.reverse.drop(1).take(2).exists { !_.isLetter  }
    val endsWithDash = line1Text.lastOption.exists(_ == '-')
    val isBrokenWord = endsWithDash && lineMinLenReq  // && !hasNonLetterPrefix

    // vtrace.trace("Broken word joined:" withInfo(word))
    // vtrace.trace("Broken word hyphenated:" withInfo(s"${w1}-${w2}"))

    def dehyphenate(): TextReflow = {
      val dehyph = line1.modifyCharAt(line1Text.length-1)({case _ => Some("")})

      join("")(dehyph, line2)
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
          val w2Extra = secondHalf.reverse.takeWhile(!_.isLetter).reverse

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


  def centerX(cb: PageAtom) = cb.region.bbox.toCenterPoint.x
  def centerY(cb: PageAtom) = cb.region.bbox.toCenterPoint.y

  def spaceWidths(cs: Seq[CharAtom]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.region.bbox.left - c1.region.bbox.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def pairwiseSpaceWidths(cs: Seq[Component]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.bounds.left - c1.bounds.right
      case _  => 0d
    })

    dists :+ 0d
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
          theComponent.zoneIndex.removeComponent(child)
        } else {
          flat += child
        }

        theComponent.setChildren(label, flat)
      }
    }

    def zoneIndex = theComponent.zoneIndex
    def vtrace = theComponent.zoneIndex.vtrace

    def left: Double  = theComponent.bounds.left
    def top: Double  = theComponent.bounds.top
    def height: Double  = theComponent.bounds.height
    def width: Double  = theComponent.bounds.width

    def hasLabel(l: Label): Boolean = theComponent.getLabels.contains(l)

    def hasAnyLabel(ls: Label*): Boolean = {
      ls.exists(theComponent.hasLabel(_))
    }

    def vdist(other: Component): Double = {
      other.bounds.bottom - theComponent.bounds.bottom

      // theComponent.bounds.toPoint(Compass.S).vdist(
      //   other.bounds.toPoint(Compass.S)
      // )
    }

    def columnContains(other: Component): Boolean = {
      val slopFactor = 4.5d // XXX test this magic number?

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

    def hasSameVCenterPoint(tolerance: Double=0.1)(other: Component) =
      theComponent.bounds.toCenterPoint.x.eqFuzzy(tolerance)(other.bounds.toCenterPoint.x)

    def hasSameLeftEdge(tolerance: Double=0.3)(other: Component) =
      theComponent.bounds.toPoint(Compass.W).x.eqFuzzy(tolerance)(other.bounds.toPoint(Compass.W).x)

    def isEqualWidth(tolerance: Double=0.1)(other: Component) =
      theComponent.bounds.width.eqFuzzy(tolerance)(other.bounds.width)


    def atoms = theComponent.queryInside(LB.PageAtom)

    def findCommonToplines(): Seq[Double] = {
      vtrace.trace(message("findCommonToplines"))
      getMostFrequentValues(vtrace)(
        atoms.map({c => c.bounds.top}),
        0.01d
      )
    }

    def findCommonBaselines(): Seq[Double] = {
      vtrace.trace(message("findCommonBaselines"))
      getMostFrequentValues(vtrace)(
        atoms.map({c => c.bounds.bottom}),
        0.01d
      )
    }

    def determineSpacingsHistResolution =  0.3d

    // List of avg distances between chars, sorted largest (inter-word) to smallest (intra-word)
    def determineSpacings(): Seq[Double] = {
      val dists = pairwiseSpaceWidths(atoms)
      val resolution = determineSpacingsHistResolution

      val hist = Histogram.histogram(dists, resolution)

      val spaceDists = hist.getFrequencies
        .sortBy(_.frequency)
        .reverse
        // .takeWhile(_.frequency > 0d)

      vtrace.trace("determine line/char spacings" withTrace vtraceHistogram(hist))

      spaceDists.map(_.value)
    }



    def labelSuperAndSubscripts(): Unit = {
      vtrace.trace(begin("labelSuperAndSubscripts()"))

      val tops = findCommonToplines()
      val bottoms = findCommonBaselines()
      val modalTop = tops.head // - 0.01d
      val modalBottom = bottoms.head // + 0.01d
      val modalCenterY = (modalBottom + modalTop)/2


      val supSubTol = (modalCenterY-modalTop) * 0.25
      val subScriptUpperLimit = modalCenterY-supSubTol
      val superScriptLowerLimit = modalCenterY+supSubTol

      // indicate a set of h-lines inside theComponent.targetRegion
      def indicateHLine(y: Double): TargetFigure = y.toHLine
        .clipTo(theComponent.targetRegion.bbox)
        .targetTo(theComponent.targetRegion.target)

      vtrace.trace(
        "modal top     " withTrace indicateHLine(modalTop),
        "modal bottom  " withTrace indicateHLine(modalBottom),
        "modal center Y" withTrace indicateHLine(modalCenterY)
      )


      theComponent.getChildren(LB.TextSpan).foreach({ textSpan =>

        val supOrSubList = textSpan.atoms.map { atom =>
          val cctr = atom.bounds.toCenterPoint
          val cbottom = atom.bounds.bottom
          val supSubTolerance = theComponent.bounds.height / 20.0

          // vtrace.trace(s"checking sup/sub, tol:${supSubTolerance.pp}" withInfo PageAtom.boundsBox(atom))

          if (atom.bounds.bottom < superScriptLowerLimit) {
            vtrace.trace((message(s"sup")))
            LB.Sup
          } else if (atom.bounds.top > subScriptUpperLimit) {
            vtrace.trace((message(s"sub")))
            LB.Sub
          } else {
            LB.CenterScript
          }
        }

        val labelSpans = supOrSubList.groupByPairs({(l1, l2) => l1 == l2 })
          .map({lls => lls.head})

        val supSubRegions = textSpan
          .groupAtomsIf({(atom1, atom2, pairIndex) =>
            val shouldGroup = supOrSubList(pairIndex) == supOrSubList(pairIndex+1)
            // vtrace.trace(message(s"groupIf ${supOrSubList.toList(pairIndex)} == ${supOrSubList.toList(pairIndex+1)}"))
            shouldGroup
          }, {(region, regionIndex) =>
            // vtrace.trace(message(s"groupIf (true) r:${region}, i:${regionIndex}"))
            region.addLabel(labelSpans(regionIndex))
          })

        textSpan.setChildren(LB.TextSpan, supSubRegions)

      })

      theComponent.ungroupChildren(LB.TextSpan)(_ => true)
      // vtrace.trace("Ending Tree" withInfo VisualLine.renderRoleTree(theComponent))

      vtrace.trace(end("labelSuperAndSubscripts()"))
    }

    def guessWordbreakWhitespaceThreshold(): Double = {
      val charDists = determineSpacings()

      val charWidths = theComponent.atoms.map(_.bounds.width)
      val widestChar = charWidths.max

      // Don't  accept a space wider than (some magic number)*the widest char?
      val saneCharDists = charDists.filter(_ < widestChar*2)
      val resolution = determineSpacingsHistResolution

      // Try to divide the list of char dists into 2 groups, small gap and large gap:

      // See if we can divide our histogram values by some value > 2*histResolution
      val distGroups = saneCharDists.groupByPairs( { (c1, c2) =>
        math.abs(c2 - c1) < resolution*1.1
      })


      val threshold = if (saneCharDists.length == 1) {
        // If there is only 1 distance, the line is only 1 word (no word breaks)
        1.0d
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

      vtrace.trace(
        "guessWordbreakWhitespaceThreshold" withInfo
          s"""| Char Dists     = ${charDists.map(_.pp).mkString(", ")}
              | Sane Dists     = ${saneCharDists.map(_.pp).mkString(", ")}
              | Widest Char    = ${widestChar.pp}
              | Threshold      = ${threshold.pp}
              |""".stripMargin.mbox
      )
      threshold
    }

    def groupTokens(): Unit = {
      vtrace.trace(begin("Split On Whitespace"))
      vtrace.trace(message(s"chars: ${theComponent.chars}"))
      // TODO assert theComponent roleLabel structure is TextSpan/TextSpan*/PageAtom

      val splitValue = guessWordbreakWhitespaceThreshold()

      theComponent.addLabel(LB.Tokenized)

      var dbgGrid = Grid.widthAligned(
        (1, AlignLeft), // join indicator
        (2, AlignLeft), // char(s)
        (6, AlignRight), // char.left
        (1, AlignLeft), // space
        (5, AlignRight) // char.width
      )


      theComponent.groupAtomsIf({ (c1, c2, pairIndex) =>

        val pairwiseDist = c2.bounds.left - c1.bounds.right
        val willGroup = pairwiseDist < splitValue

        vtrace.ifTrace({
          dbgGrid = dbgGrid.addRow(
            if(willGroup) "_" else "$",
            c1.chars,
            c1.bounds.left.pp,
            "~",
            c1.bounds.width.pp
          )
        })

        willGroup

      }, { (newRegion, regionIndex) =>
        newRegion.addLabel(LB.Token)
        theComponent.addChild(LB.TextSpan, newRegion)
      })

      vtrace.trace({
        "line groups" withInfo dbgGrid.toBox().transpose()
      })

      // vtrace.trace("Tree after split whitespace" withInfo VisualLine.renderRoleTree(theComponent))

      vtrace.trace(end("Split On Whitespace"))
    }


    def tokenizeLine(): Unit = {
      if (!theComponent.hasLabel(LB.TokenizedLine)) {
        theComponent.addLabel(LB.TokenizedLine)

        vtrace.trace(begin("Tokenize Line"), focusOn(theComponent.targetRegion))

        // vtrace.trace(message(s"Line chars: ${theComponent.chars}"))

        labelSuperAndSubscripts()
        // Structure is: VisualLine/TextSpan/[TextSpan[sup/sub/ctr]]
        // Group each TextSpan w/sup/sub/ctr-script label into tokens
        theComponent.getChildren(LB.TextSpan)
          .filter(_.hasAnyLabel(LB.CenterScript, LB.Sub, LB.Sup))
          .foreach{ _.groupTokens() }

        theComponent.addLabel(LB.Tokenized)

        // Now figure out how the super/sub/normal text spans should be joined together token-wise
        // vtrace.trace("Tree after _.groupTokens()" withInfo VisualLine.renderRoleTree(theComponent))

        theComponent.ungroupChildren(LB.TextSpan)({c =>
          if (c.hasLabel(LB.Sup) || c.hasLabel(LB.Sub)) {
            c.addLabel(LB.Token)
          }
          c.hasLabel(LB.CenterScript)
        })

        // vtrace.trace("Tree after ungroupChildren()" withInfo VisualLine.renderRoleTree(theComponent))

        // TODO don't compute this multiple times
        val splitValue = guessWordbreakWhitespaceThreshold()

        theComponent.groupChildren(withLabel=LB.TextSpan, newLabel=LB.TextSpan)(
          {(c1, c2, pairIndex) =>
            val pairwiseDist = c2.bounds.left - c1.bounds.right

            // vtrace.trace("token grouping" withInfo vcat(Seq(
            //   hcat(center1)(Seq(PageAtom.boundsBox(c1) + " <-> " + PageAtom.boundsBox(c2))),
            //   s"""|  pairwisedist: ${pairwiseDist.pp}  east-west dist: ${pairwiseDist.pp}
            //       |  split value: ${splitValue},
            //       |  Will split? : ${pairwiseDist < splitValue}
            //       |""".stripMargin.mbox
            // )))

            pairwiseDist < splitValue
          },{(region, regionIndex) =>
            region.addLabel(LB.Token)
          }
        )
        // vtrace.trace("Tree after regrouping tokens" withInfo VisualLine.renderRoleTree(theComponent))

        theComponent.ungroupChildren(LB.TextSpan)({c =>
          c.getChildren(LB.TextSpan).length == 1
        })

        // vtrace.trace("Tree after Final ungrouping " withInfo VisualLine.renderRoleTree(theComponent))

        val maybeReflow = CVisualLine.toTextReflow(theComponent)

        maybeReflow.foreach {
          theComponent.setTextReflow(_)
        }


        vtrace.trace("Final Tokenization" withInfo
          maybeReflow.map(_.toText().box).getOrElse("<no text>".box))

        vtrace.trace(end("Tokenize Line"))
      }
    }


    def determineNormalTextBounds: LTBounds = {
      val mfHeights = Histogram.getMostFrequentValues(vtrace)(theComponent.atoms.map(_.bounds.height), 0.1d)
      val mfTops = Histogram.getMostFrequentValues(vtrace)(theComponent.atoms.map(_.bounds.top), 0.1d)


      val mfHeight= mfHeights.headOption.getOrElse(0d)
      val mfTop = mfTops.headOption.getOrElse(0d)

      theComponent.atoms
        .map({ c =>
          val cb = c.bounds
          LTBounds(
            left=cb.left, top=mfTop,
            width=cb.width, height=mfHeight
          )
        })
        .foldLeft(theComponent.atoms.head.bounds)( { case (b1, b2) =>
          b1 union b2
        })
    }

    import textflow.TextReflow._

    def setTextReflow(r: TextReflow): Unit = {
      theComponent.zoneIndex.setTextReflow(theComponent, r)
    }

    def getTextReflow(): Option[TextReflow]= {
      theComponent.zoneIndex.getTextReflow(theComponent.id)
    }
  } // RicherComponent
}
