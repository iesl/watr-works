package edu.umass.cs.iesl.watr
package segment

import watrmarks._
import geometry._
import geometry.syntax._
import textboxing.{TextBoxing => TB}
import textgrid._

import segment.{SegmentationLabels => LB}

trait LineShapeClassification extends PageScopeSegmenter { self =>
  lazy val lineShapes = self


  def classifyLines(): Unit = {
    // groupLines()
    // showGroupings()
  }


  implicit class RicherBoolean(val self: Boolean) {
    def implies(action: => Unit) = {
      if (self) {
        val _ = action
      }
    }

    def ==>(action: => Unit) = implies(action)
  }

  private def getTrapezoidAttr(cc: Int@@ShapeID): Option[Trapezoid] = {
    pageIndex.shapes.getShapeAttribute[Trapezoid](cc, watrmarks.Label("Trapezoid"))
  }

  private def getWeightsAttr(cc: Int@@ShapeID): Option[WeightedLabeling] = {
    pageIndex.shapes.getShapeAttribute[WeightedLabeling](cc, watrmarks.Label("LineGrouping"))
  }

  private def getComponentText(cc: Int@@ShapeID, l: Label): Option[TextGrid.Row] = {
    // val l1TextRowOpt = pageIndex.shapes.getComponentText(line1CC, LB.VisualLine)
    ???
  }

  // private def groupLines(): Unit = {
  //   for {
  //     lineShape <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
  //     _ = {
  //       // Create weighted label set on each line
  //       lineCCs.foreach { cc =>
  //         pageIndex.shapes.setShapeAttribute(cc.id, watrmarks.Label("LineGrouping"), WeightedLabeling())
  //       }
  //     }
  //     linePair <- lineCCs.sliding(3)
  //   }  {
  //     import LB._

  //     linePair match {

  //       case Seq(line1CC, line2CC, line3CC) =>
  //         val l1TextRowOpt = getComponentText(line1CC, LB.VisualLine)
  //         val l2TextRowOpt = getComponentText(line2CC, LB.VisualLine)
  //         val l3TextRowOpt = getComponentText(line3CC, LB.VisualLine)


  //         val l1Trapezoid = getTrapezoidAttr(line1CC.id)
  //         val l2Trapezoid = getTrapezoidAttr(line2CC.id)

  //         val l1Labels = getWeightsAttr(line1CC.id).get
  //         val l2Labels = getWeightsAttr(line2CC.id).get
  //         val l3Labels = getWeightsAttr(line3CC.id).get


  //         object line1 { def +=(p: BioPin): Unit = { l1Labels.addPin(p) } }

  //         object line1_2_3 {
  //           def +=(ps: (BioPin, BioPin, BioPin)): Unit = {
  //             l1Labels.addPin(ps._1)
  //             l2Labels.addPin(ps._2)
  //             l3Labels.addPin(ps._3)
  //           }
  //         }


  //         (l1Trapezoid, l2Trapezoid) match {
  //           case (Some(t1), Some(t2)) =>
  //             val shape1 = ShapeProps(t1, l1TextRowOpt, l2TextRowOpt)
  //             val shape2 = ShapeProps(t2, l2TextRowOpt, l3TextRowOpt)


  //             (shape1.like.anyParaBegin     && shape2.like.paraInside)       ==> { line1_2_3 += ((Para.B, Para.I, Para.I)) };
  //             (shape1.like.paraInside       && shape2.like.paraInside)       ==> { line1_2_3 += ((Para.I, Para.I, Para.I)) };
  //             (shape1.like.paraInside       && shape2.like.paraLast)         ==> { line1_2_3 += ((Para.I, Para.I, Para.L)) };
  //             (shape1.like.paraLastAndBegin && shape2.like.anyParaBegin)     ==> { line1_2_3 += ((Para.L, Para.B, Para.I)) };
  //             (shape1.like.paraLast         && shape2.like.paraLastAndBegin) ==> { line1_2_3 += ((Para.I, Para.L, Para.B)) };

  //             shape1.textLike.captionBegin ==> {  line1 += Caption.B }

  //             //
  //             shape1.upperText


  //           case _ =>

  //         }



  //       case Seq(line1CC, line2CC) =>
  //         val l1TextRowOpt = getComponentText(line1CC, LB.VisualLine)
  //         val l2TextRowOpt = getComponentText(line2CC, LB.VisualLine)

  //         // val l1Text = l1TextRowOpt.map(_.toText()).getOrElse("<no text>")
  //         // val l2Text = l2TextRowOpt.map(_.toText()).getOrElse("<no text>")
  //         val l1Trapezoid = getTrapezoidAttr(line1CC.id)
  //         val l2Trapezoid = getTrapezoidAttr(line2CC.id)
  //         val l1Labels = getWeightsAttr(line1CC.id).get
  //         val l2Labels = getWeightsAttr(line2CC.id).get


  //         object line1 { def +=(p: BioPin): Unit = { l1Labels.addPin(p) } }
  //         // object line2 { def +=(p: BioPin): Unit = { l2Labels.addPin(p) } }

  //         object line1_2 {
  //           def +=(ps: (BioPin, BioPin)): Unit = {
  //             l1Labels.addPin(ps._1)
  //             l2Labels.addPin(ps._2)
  //           }
  //         }


  //         // println(s"> ${l1Text}")
  //         // println(s"> ${l2Text}")
  //         // println(s"     ${l1Trapezoid.map(_.prettyPrint)}")
  //         // println(s"     ${l2Trapezoid.map(_.prettyPrint)}")


  //         (l1Trapezoid, l2Trapezoid) match {
  //           case (Some(t1), Some(t2)) =>
  //             val shape1 = ShapeProps(t1, l1TextRowOpt, l2TextRowOpt)
  //             val shape2 = ShapeProps(t2, l2TextRowOpt, None)


  //             shape1.like.paraInside        ==>  { line1 += Para.I }
  //             shape1.like.paraLast          ==>  { line1_2 += (Para.I -> Para.L) }
  //             shape1.like.dropCapParaBegin  ==>  { line1_2 += (Para.B -> Para.I) }
  //             shape1.like.indentedParaBegin ==>  { line1_2 += (Para.B -> Para.I) }
  //             shape1.like.blockParaBegin    ==>  { line1 += Para.B }
  //             shape1.like.paraLastAndBegin  ==>  { line1_2 += (Para.L -> Para.B) }
  //             shape1.textLike.captionBegin  ==>  { line1_2 += (Para.L -> Para.B) }


  //             (shape1.like.anyParaBegin     && shape2.like.paraInside)       ==> { line1_2 += (Para.B -> Para.I)  };
  //             (shape1.like.paraInside       && shape2.like.paraInside)       ==> { line1_2 += (Para.I -> Para.I)  };
  //             (shape1.like.paraInside       && shape2.like.paraLast)         ==> { line1_2 += (Para.I -> Para.I)  };
  //             (shape1.like.paraLastAndBegin && shape2.like.anyParaBegin)     ==> { line1_2 += (Para.L -> Para.B)  };
  //             (shape1.like.paraLast         && shape2.like.paraLastAndBegin) ==> { line1_2 += (Para.I -> Para.L)  };

  //             // println(s"""l1 labels: ${l1Labels.countedPins().map({case (p, c) => s"$p(${c})" }).mkString}""")


  //           case (Some(t1), None) =>
  //             val shape1 = ShapeProps(t1, l1TextRowOpt, None)

  //             shape1.like.paraInside        ==>  { line1 += Para.I }
  //             shape1.like.paraLast          ==>  { line1_2 += (Para.I -> Para.L) }
  //             shape1.like.dropCapParaBegin  ==>  { line1_2 += (Para.B -> Para.I) }
  //             shape1.like.indentedParaBegin ==>  { line1_2 += (Para.B -> Para.I) }
  //             shape1.like.blockParaBegin    ==>  { line1 += Para.B }
  //             shape1.like.paraLastAndBegin  ==>  { line1_2 += (Para.L -> Para.B) }

  //           case (None, Some(t2)) =>
  //           case (None, None) =>
  //         }


  //       case _ =>

  //     }
  //   }
  // }

  // def showGroupings(): Unit = {
  //   import TB._
  //   val textCol = for {
  //     (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
  //     lineCC <- lineCCs
  //   } yield {

  //     val lineText = getComponentText(lineCC, LB.VisualLine).map(_.toText().take(40).mkString)
  //     lineText.getOrElse("<no text>").box
  //   }

  //   val pinCol = for {
  //     (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
  //     lineCC <- lineCCs
  //   } yield {
  //     val lineWeights = pageIndex.shapes.getShapeAttribute[WeightedLabeling](lineCC.id, watrmarks.Label("LineGrouping")).get

  //     val pinstr = lineWeights.countedPins().map({case (p, c) => s"$p(${c})" }).mkString("; ")
  //     // val pins = lineWeights.uniquePins()
  //     // val pinstr = pins.mkString(", ")

  //     pinstr.box
  //   }

  //   val groupings = hjoin(
  //     vjoins(TB.right, pinCol),
  //     "  ",
  //     vjoins(TB.left, textCol)
  //   )

  //   println()
  //   println(groupings)
  //   println()

  // }

  // object trapPairProps {
  //   val areLeftAligned: Boolean = true
  //   val areRightAligned: Boolean = true
  //   val areCenterAligned: Boolean = true

  //   object text {
  //     val isAlphabeticallyOrdered: Boolean = true
  //     val isNumericallyOrdered: Boolean = true
  //   }
  // }


  object TextLineProps {
    def apply(textRow: Option[TextGrid.Row]): TextLineProps = new TextLineProps {
      def hasText = textRow.isDefined
      def text = textRow.map(_.toText()).getOrElse("")

      def isCommonFont: Boolean = true
      def hasLeadingUncommonFont: Boolean = true
      def hasLeadingNonUniqueTextFont: Boolean = true
      def hasNaturalLanguageCharDistribution: Boolean = true

      def hasTrailingParenNumberingPattern: Boolean = true // e.g., (2) as in numbered formulae
      def hasLeadingSquareBracketNumberingPattern: Boolean = true // e.g., [3] as in ref markers

      def hasEqualSign: Boolean = true //

      def splitFont: Boolean = false

    }
  }

  trait TextLineProps {

    def hasText: Boolean
    def text: String

    def isCommonFont: Boolean
    def hasLeadingUncommonFont: Boolean
    def hasLeadingNonUniqueTextFont: Boolean
    def hasNaturalLanguageCharDistribution: Boolean

    def hasTrailingParenNumberingPattern: Boolean // e.g., (2) as in numbered formulae
    def hasLeadingSquareBracketNumberingPattern: Boolean // e.g., [3] as in ref markers

    def hasEqualSign: Boolean

    def splitFont: Boolean
  }

  object ShapeProps {

    /***
     *  Ideas:
     *  + bi/trigrams for common char patterns to classify math vs chem vs text regions
     *  + Font properties:
     *    - most common (text|symbol)
     *    - most common non-common-font appearing at line beginnings (inline header, Fig, Table. etc.)
     *    - most common non-common-font appearing at page extrema (journal headers, page#s, footers)
     *    - most common non-common-font appearing in references (emphasized, bold or italic)
     *    - most common non-common-font appearing as complete lines
     *
     * + Text Properties
     *   - Table/figure text tends to occur at extreme top/bottom of page
     *    - find predominant page top/bottom fonts
     *
     *
     *  - Shape properties
     *    - Most common para indent shape, as left-angle, e.g., 30 deg  ◿◻◻
     *    - Most common hanging indent shape left-angle, for refs
     *    - Most common para end/begin left angle
     *    - Inset math shape (like para end without a final period)
     *
     * + Labels
     *    - graph axis labels
     *
     **/

    def apply(
      t: Trapezoid,
      l1TextOpt: Option[TextGrid.Row],
      l2TextOpt: Option[TextGrid.Row]
    ): ShapeProps = {
      val (llAngleType, lrAngleType) = t.classifyBaseAngles()
      val (llAngle, lrAngle) = (t.leftBaseAngle(), t.rightBaseAngle())

      def hasAngles(l: AngleType, r: AngleType) = l==llAngleType && r==lrAngleType

      new ShapeProps {
        def isModalHeight: Boolean = false
        def isModalWidth: Boolean = false
        def isRectangular: Boolean = hasAngles(AngleType.Right, AngleType.Right)
        def isIsosceles: Boolean = false

        def like: ShapeResemblance = new ShapeResemblance {
          def dropCapParaBegin: Boolean  = false
          def blockParaBegin: Boolean    = hasAngles(AngleType.Right, AngleType.Right)
          def indentedParaBegin: Boolean = hasAngles(AngleType.Acute, AngleType.Right)
          def hangingParaBegin: Boolean  = false
          def anyParaBegin: Boolean      = dropCapParaBegin || blockParaBegin || indentedParaBegin || hangingParaBegin
          def paraInside: Boolean        = isRectangular
          def paraLast: Boolean          = hasAngles(AngleType.Right, AngleType.Obtuse)
          def paraLastAndBegin: Boolean  = hasAngles(AngleType.Obtuse, AngleType.Acute)
        }


        def upperText = TextLineProps(l1TextOpt)
        def lowerText = TextLineProps(l2TextOpt)

        def textLike: TextResemblance = new TextResemblance {
          def captionBegin = {
            val lowerChars = upperText.text.replaceAll(" ", "").toLowerCase()

            (upperText.hasText &&
              (lowerChars.startsWith("fig")
                || lowerChars.startsWith("table")))

          }
        }
      }

    }
  }

  trait ShapeResemblance {
    def dropCapParaBegin: Boolean
    def blockParaBegin: Boolean
    def indentedParaBegin: Boolean
    def hangingParaBegin: Boolean
    def anyParaBegin: Boolean
    def paraInside: Boolean
    def paraLast: Boolean
    def paraLastAndBegin: Boolean

  }

  trait TextResemblance {
    def captionBegin: Boolean
  }


  trait ShapeProps {
    def isModalHeight: Boolean
    def isModalWidth: Boolean
    def isRectangular: Boolean
    def isIsosceles: Boolean

    def like: ShapeResemblance

    def upperText: TextLineProps
    def lowerText: TextLineProps

    def textLike: TextResemblance

  }
}

