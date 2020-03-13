package org.watrworks
package segment

import watrmarks._
import geometry._
import geometry.syntax._
import textgrid._
import TypeTags._

trait LineShapeClassification extends PageScopeSegmenter { self =>
  lazy val lineShapes = self


  def classifyLines(): Unit = {
    addLineWeights()
  }


  implicit class RicherBoolean(val self: Boolean) {
    def implies(action: => Unit) = {
      if (self) {
        val _ = action
      }
    }

    def ==>(action: => Unit) = implies(action)
  }

  def getTraps(reprShapes: AnyShape*): List[Option[Trapezoid]] = {
    reprShapes.toList.map(
      getTrapezoidForShape(_)
    )
  }

  def getTextRows(reprShapes: AnyShape*): List[Option[TextGrid.Row]] = {
    reprShapes.toList.map(
      getTextForShape(_)
    )
  }

  def getWeights(reprShapes: AnyShape*): List[WeightedLabeling] = {
    reprShapes.toList.map(
      getWeightsForShape(_).get
    )
  }

  import LB._

  private def addLineWeights(): Unit = {
    val lineReprShapes = getLabeledRects(LB.BaselineMidriseBand)
    val shapeAndCharsAndScaledFontId = lineReprShapes
      .map(l => (l, getCharsForShape(l)))
      .sortBy { case (_, baselineChars) =>
        baselineChars.head.id
      }

    shapeAndCharsAndScaledFontId.sliding(2).foreach { case linePair =>
      linePair match {

        case Seq(l1, l2, l3) =>
          val (line1ReprShape, line1Chars) = l1
          val (line2ReprShape, line2Chars) = l2
          val (line3ReprShape, line3Chars) = l3

          val List(line1Text, line2Text, line3Text) =
            getTextRows(line1ReprShape, line2ReprShape, line3ReprShape)

          val List(l1Trapezoid, l2Trapezoid) =
            getTraps(line1ReprShape, line2ReprShape)

          val List(l1Labels, l2Labels, l3Labels) =
            getWeights(line1ReprShape, line2ReprShape, line3ReprShape)

          object line1 { def +=(p: BioPin): Unit = { l1Labels.addPin(p) } }


          object line1_2_3 {
            def +=(ps: (BioPin, BioPin, BioPin)): Unit = {
              l1Labels.addPin(ps._1)
              l2Labels.addPin(ps._2)
              l3Labels.addPin(ps._3)
            }
          }


          (l1Trapezoid, l2Trapezoid) match {
            case (Some(t1), Some(t2)) =>
              val shape1 = ShapeProps(t1, line1Text, line2Text)
              val shape2 = ShapeProps(t2, line2Text, line3Text)


              (shape1.like.anyParaBegin     && shape2.like.paraInside)       ==> { line1_2_3 += ((Para.B, Para.I, Para.I)) };
              (shape1.like.paraInside       && shape2.like.paraInside)       ==> { line1_2_3 += ((Para.I, Para.I, Para.I)) };
              (shape1.like.paraInside       && shape2.like.paraLast)         ==> { line1_2_3 += ((Para.I, Para.I, Para.L)) };
              (shape1.like.paraLastAndBegin && shape2.like.anyParaBegin)     ==> { line1_2_3 += ((Para.L, Para.B, Para.I)) };
              (shape1.like.paraLast         && shape2.like.paraLastAndBegin) ==> { line1_2_3 += ((Para.I, Para.L, Para.B)) };

              shape1.textLike.captionBegin ==> {  line1 += Caption.B }

              // shape1.upperText


            case _ =>

          }



        case Seq(l1, l2) =>
          val (line1ReprShape, line1Chars) = l1
          val (line2ReprShape, line2Chars) = l2

          val List(line1Text, line2Text) =
            getTextRows(line1ReprShape, line2ReprShape)

          val List(l1Trapezoid, l2Trapezoid) =
            getTraps(line1ReprShape, line2ReprShape)

          val List(l1Labels, l2Labels) =
            getWeights(line1ReprShape, line2ReprShape)

          object line1 { def +=(p: BioPin): Unit = { l1Labels.addPin(p) } }

          object line1_2 {
            def +=(ps: (BioPin, BioPin)): Unit = {
              l1Labels.addPin(ps._1)
              l2Labels.addPin(ps._2)
            }
          }

          // println(s"Line Pairs")
          // println(s"> ${line1Text.map(_.toText())}")
          // println(s"> ${line2Text.map(_.toText())}")
          // println(s"     ${l1Trapezoid.map(_.prettyPrint)}")
          // println(s"     ${l2Trapezoid.map(_.prettyPrint)}")
          // println()


          (l1Trapezoid, l2Trapezoid) match {
            case (Some(t1), Some(t2)) =>
              val shape1 = ShapeProps(t1, line1Text, line2Text)
              val shape2 = ShapeProps(t2, line2Text, None)


              shape1.like.paraInside        ==>  { line1 += Para.I }
              shape1.like.paraLast          ==>  { line1_2 += (Para.I -> Para.L) }
              shape1.like.dropCapParaBegin  ==>  { line1_2 += (Para.B -> Para.I) }
              shape1.like.indentedParaBegin ==>  { line1_2 += (Para.B -> Para.I) }
              shape1.like.blockParaBegin    ==>  { line1 += Para.B }
              shape1.like.paraLastAndBegin  ==>  { line1_2 += (Para.L -> Para.B) }
              shape1.textLike.captionBegin  ==>  { line1_2 += (Para.L -> Para.B) }


              (shape1.like.anyParaBegin     && shape2.like.paraInside)       ==> { line1_2 += (Para.B -> Para.I)  };
              (shape1.like.paraInside       && shape2.like.paraInside)       ==> { line1_2 += (Para.I -> Para.I)  };
              (shape1.like.paraInside       && shape2.like.paraLast)         ==> { line1_2 += (Para.I -> Para.I)  };
              (shape1.like.paraLastAndBegin && shape2.like.anyParaBegin)     ==> { line1_2 += (Para.L -> Para.B)  };
              (shape1.like.paraLast         && shape2.like.paraLastAndBegin) ==> { line1_2 += (Para.I -> Para.L)  };


            case (Some(t1), None) =>
              val shape1 = ShapeProps(t1, line1Text, None)

              shape1.like.paraInside        ==>  { line1 += Para.I }
              shape1.like.paraLast          ==>  { line1_2 += (Para.I -> Para.L) }
              shape1.like.dropCapParaBegin  ==>  { line1_2 += (Para.B -> Para.I) }
              shape1.like.indentedParaBegin ==>  { line1_2 += (Para.B -> Para.I) }
              shape1.like.blockParaBegin    ==>  { line1 += Para.B }
              shape1.like.paraLastAndBegin  ==>  { line1_2 += (Para.L -> Para.B) }

            case (None, Some(t2)) =>
            case (None, None) =>
          }


        case _ =>

      }
    }


    // println(s"Final Line Labels")
    // shapeAndCharsAndScaledFontId.foreach { case l1 =>
    //   val (line1ReprShape, line1Chars) = l1
    //   val List(line1Text) = getTextRows(line1ReprShape)
    //   val List(l1Trapezoid) = getTraps(line1ReprShape)
    //   val List(l1Labels) = getWeights(line1ReprShape)
    //   println(s"> ${line1Text.map(_.toText())}")
    //   println(s"     ${l1Trapezoid.map(_.prettyPrint)}")
    //   println(s"""   ${l1Labels.countedPins().map({case (p, c) => s"$p(${c})" }).mkString}""")
    //   println()
    // }

  }


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
     * + Identifying notes associated with author names (institutions, etc)
     *   - for each identified superscript symbol, put a column-evidence dot on page, then try
     *     to draw a column identifying the notes
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
