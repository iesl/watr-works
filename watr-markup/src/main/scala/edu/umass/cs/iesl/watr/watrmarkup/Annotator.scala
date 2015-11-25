package edu.umass.cs.iesl.xml_annotator

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.Writer

import org.jdom2.Content
import org.jdom2.input.SAXBuilder
import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable

import scala.collection.JavaConversions.iterableAsScalaIterable
import scala.collection.immutable.IntMap
import scala.collection.immutable.Queue
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedSet

import org.jdom2.output.Format
import org.jdom2.output.XMLOutputter
import org.jdom2.output.LineSeparator
import org.jdom2.output.support.AbstractXMLOutputProcessor

/** Commentary definitions
  *
  * block index - the position of a non-empty tspan out of all non-empty tspans
  * char index - the position of a character out of the characters in an element's text
  * total index - the position of a character out of all the characters in all the non-empty tspans
  * index pair - a block index and a char index
  * b-index pair - index pair that has a B or U label for some annotation type
  * label map - a map of char indexes to labels
  * segment - a map of block indexes to label maps
  *         - such that either there is only one label and it is a U
  *         - or the label at the lowest index pair is B and the label at the highest is L
  * text map - a map from block indexes to pairs of char index and text
  *          - where the char index is that of the first char of the text
  */

/** Container of functions and constructors
  *
  * It is used by Annotator instances and is to be used on results from Annotator isntances
  */
object Annotator {

  /** Constructor for annotation types
    *
    * It is used by Annotator instances to correlate annotation type names
    * with constraint ranges
    */
  case class AnnotationType(name: String, c: Char, constraintRange: ConstraintRange)


  case class Annotation(annoType: AnnotationType, content: AnnotationContent) {

    override def toString: String = {

      def toStringWithIndent(annotation: Annotation, indent: String): String = {
        indent + annotation.annoType.name + ": \n" + (annotation.content match {
          case LeafContent(xs) =>
            xs.map(x => indent + "  " + x._1 + " -> " + x._2).mkString("\n")
          case InnerContent(xs) =>
            xs.map(a => toStringWithIndent(a, indent + "  ")).mkString("\n")
        })
      }

      toStringWithIndent(this, "")
    }

  }

  sealed trait AnnotationContent
  case class LeafContent(xs: List[(Int, String)]) extends AnnotationContent
  case class InnerContent(xs: List[Annotation]) extends AnnotationContent



  /** annotation link
    *
    * It is used by Annotator instances to hold
    * a map of annotation type strings to b-indexes of their keys' annotation type
    */
  case class AnnotationLink(name: String, attrValueMap: Map[String, (String, Int)])


  /** Constructor for annotation spans
    *
    * It is used by Annotator instances to hold label maps that pertain
    * to a particular tspan element and have labels of annotation types
    * specified in the provided annotation type sequence
    */
  case class AnnotationSpan(
      labelMap: IntMap[Label],
      /** sequence of annotation types that can be associated with labels in labelMap **/
      annotationTypeSeq: Seq[AnnotationType]
  )

  /** Constructor for annotation infos
    *
    * It is used by Annotator instances to associate
    * b-index pairs with an annotation type
    */
  case class AnnotationInfo(
      annotationType: AnnotationType,
      bIndexSortedSet: SortedSet[Int]
  )

  /** Constructor for annotation blocks
    *
    * It used by Annotator instances to hold
    * the total index of the first characater of a particular tspan element as the startIndex
    * and the total index of first character of the next tspan as the nextIndex
    * and a map of annotation types to annotation spans that exist
    * for the tspan element
    **/
  case class AnnotationBlock(
      startIndex: Int,
      nextIndex: Int,
      annotationMap: ListMap[AnnotationType, AnnotationSpan]
  )

  /** Constructor for position groups
    *
    * It used by Annotator instances to hold
    * the x and y positions of every character in a particular tspan
    * and the x position of the right side of the last character
    **/
  case class PositionGroup(xs: List[Double], endX: Double, ys: List[Double])



  import SvgMatrix._

  /** Function to get source element's x and y positions in its ancestor's coordinate system **/
  def getTransformedCoords(sourceE: Element, ancestorE: Element): PositionGroup = {

    def matrixTotal(e: Element): SvgMatrix = {
      require(e != null)
      val m = svgMatrix(e)
      if (e == ancestorE) {
        m
      } else {
        svgMatrixMultiply(matrixTotal(e.getParentElement()), m)
      }
    }

    val m = matrixTotal(sourceE)
    val sourceXs = Attr.xs(sourceE)
    val sourceY = Attr.y(sourceE)

    val _xs = sourceXs.map(x => {
      m(0) * x + m(2) * sourceY + m(4)
    })

    val _ys = sourceXs.map(x => {
      m(1) * x + m(3) * sourceY + m(5)
    })

    val _endX = m(0) * Attr.endX(sourceE) + m(2) * sourceY + m(4)

    PositionGroup(_xs.toList, _endX, _ys.toList)

  }


  final def mkBreakMap(size: Int, breakIndexSet: Set[Int]): Map[Int, Int] = {
    (0 until size).foldLeft(Map[Int, Int]()) { case (mapAcc, index) =>
      val key = if (mapAcc.isEmpty) 0 else mapAcc.keySet.max + 1
      if (breakIndexSet.contains(index)) {
        mapAcc + ((key + 1) -> index)
      } else {
        mapAcc + (key -> index)
      }
    }
  }

  /** Function to make a string of text with specified characters inserted at specified locations **/
  final def mkTextWithBreaks(text: String, breakIndexSet: Set[Int], break: Char = '\n'): String = {

    text.toList.zipWithIndex.foldLeft("") {
      case (strAcc, (char, index)) =>
        if (breakIndexSet.contains(index)) {
          strAcc + break + char
        } else {
          strAcc + char
        }
    }
  }

  def parseBioLabels(labelString: String): Map[Int, Label] = {
    (labelString
      .toIndexedSeq.zipWithIndex
      .filter(p => p._1 != ' ').map {
      case (c, i) =>
        i -> (c match {
          case '-' => O
          case '~' => I
          case '$' => L
          case t if t.isLower => B(t)
          case t if t.isUpper => U(t.toLower)
        })
    }).toMap
  }

  def parseBioTypes(typeString: String): List[(String, Char)] = {
    typeString.split(", ").map(pairString => {
      val Array(str, c) = pairString.split(": ")
      (str, c.toCharArray()(0))
    }).toList
  }

  def parseBioConstraints(constraintString: String): ConstraintRange = {
    val typeList = constraintString.split('.')
    val size = typeList.size
    val lastType = typeList(size - 1)
    val con = if (lastType == "char") {
      CharCon
    } else {
      SegmentCon(lastType)
    }

    if (size == 1) {
      Single(con)
    } else {
      Range(typeList(0), con)
    }
  }

  def parseBioBlock(blockString: String): List[(Map[Int, Label], List[(String, Char)], ConstraintRange)] = {
    val labelPattern = """\| \|([a-zA-Z~ \$]+)\| """
    val typePattern = """[a-z\-]+: [a-z]"""
    val constraintPattern = """[a-z\-]+"""
    val fullPattern = labelPattern + """\{type: \{(""" + typePattern + "(, " + typePattern + ")*" +
                      """)\}, unit: (""" + constraintPattern + """(\.""" + constraintPattern + ")*" +""")\}"""

    fullPattern.r.findAllIn(blockString).toList.reverse.map(spanString => {
      spanString match {
        case fullPattern.r(labelString, typeString, _, constraintString, _) =>

          val labelMap = parseBioLabels(labelString)
          val typePairList = parseBioTypes(typeString)
          val constraintRange = parseBioConstraints(constraintString)
          (labelMap, typePairList, constraintRange)
      }
    })

  }

  def parseBioDoc(es: List[Element]): List[List[(Map[Int, Label], List[(String, Char)], ConstraintRange)]] = {
    es.map(e => {
      e.getAttribute("bio") match {
        case null => List()
        case attr =>
          val bioString = attr.getValue()
          parseBioBlock(bioString)
      }
    })
  }

  /** Public constructor
    *
    * It populates the annotation block sequence by creating
    * an annotation block for every non empty tspan it finds
    */
  def apply(dom: Document, loadAnnotations: Boolean = false): Annotator = {
    val cDom = dom.clone()
    val annoLinksEs = cDom.getRootElement().getDescendants(new ElementFilter("annotation-links")).toIterable.toList
    annoLinksEs.foreach(_.detach())
    val anno = new Annotator(
      cDom,
      DOMUtils.getTSpanElements(cDom).foldLeft(IndexedSeq[AnnotationBlock]())( (seqAcc, e) => {
        val startIndex = if (seqAcc.isEmpty) 0 else seqAcc.last.nextIndex
        val nextIndex = startIndex + e.getText().size
        seqAcc :+ AnnotationBlock(startIndex, nextIndex, ListMap())
      } ),
      HashMap(),
      HashSet()
    )

    if (loadAnnotations) {

      val parsing: Seq[Seq[(Map[Int, Label], List[(String, Char)], ConstraintRange)]] = {
        parseBioDoc(anno.getElements().toList)
      }

      val partialAnnotationSeq: Seq[(Map[(Int, Int), Label], List[(String, Char)], ConstraintRange)] = {
        parsing.toIndexedSeq.zipWithIndex.flatMap {
          case (xs, blockIndex) =>
            xs.map {
              case (labelMap, typeList, constraintRange) =>
                val pairIndexLabelMap = labelMap.map {
                  case (charIndex, label) => ((blockIndex, charIndex), label)
                }
                (pairIndexLabelMap, typeList, constraintRange)
            }
        }
      }

      val annotationMap = partialAnnotationSeq.foldLeft(HashMap[(List[(String, Char)], ConstraintRange), Map[(Int, Int), Label]]()) {
        case (mapAcc, (pairIndexLabelMap, typeList, constraintRange)) =>
          val key = (typeList, constraintRange)

          if (mapAcc.contains(key)) {
            mapAcc + (key -> (mapAcc(key) ++ pairIndexLabelMap))
          } else {
            mapAcc + (key -> pairIndexLabelMap)
          }
      }

      val orderedKeySeq = partialAnnotationSeq.map {
        case (_, typeList, constraintRange) =>
          (typeList, constraintRange)
      } distinct

      val orderedAnnotationSeq = orderedKeySeq.map(k => {
        val (typeList, constraintRange) = k
        val indexPairMap = annotationMap(k)
        (typeList, constraintRange, indexPairMap)

      })

      val annotationLinks = annoLinksEs.flatMap(annoLinksE => {
        annoLinksE.getChildren().map(e => {
          val name = e.getName()
          val attrList = e.getAttributes()
          val attrValueMap = attrList.map(attr => {
            val attrName = attr.getName()
            val Array(typeString, totalIndexString) = attr.getValue().split(' ')


            (attrName, (typeString, totalIndexString.toInt))

          }).toMap

          AnnotationLink(name, attrValueMap)
        })
      }).toSet

      (orderedAnnotationSeq.foldLeft(anno) {
        case (annoAcc, (annoTypePairList, constraintRange, indexPairMap)) =>
          annoAcc.annotateWithIndexPairMap(annoTypePairList, constraintRange, indexPairMap)
      }).annotateLink(annotationLinks)

    } else {
      anno
    }

  }


}

import Annotator._
/**
  * Private Constructor for Annotator, which stores annotations associated with svg data
  *
  * instances have methods and attributes for retrieving annotation and svg data and
  * adding new annotations
  */
class Annotator private (
    /** Original mutable object containing svg data**/
    private val dom: Document,
    /** Sequence of annotation blocks
      *
      * Each annotation block's position is a block index
      */
    val annotationBlockSeq: IndexedSeq[AnnotationBlock],
    /** map of annotation type strings to annotation infos **/
    val annotationInfoMap: Map[String, AnnotationInfo],
    /** Set of annotation links
      *
      * Each AnnotationLink is a map of annotation type strings to an index pairs
      * where the index pair has the begin (B) or unit (U) label of the annotation type
      */
    val annotationLinkSet: Set[AnnotationLink]
) {

  /** Clone of dom, who and whose elements are never passed outside of instance and who is always unmutated **/
  private val frozenDom = dom.clone()
  private val frozenElementSeq = DOMUtils.getTSpanElements(frozenDom).toIndexedSeq

  /** Clone of dom for passing outside **/
  private var _dom: Document = frozenDom.clone()
  private var _elementSeq: Seq[Element] = DOMUtils.getTSpanElements(_dom).toIndexedSeq

  /** Function to replace _dom with a certainly clean unmutated dom **/
  final def resetDom(): Unit = {
    _dom = frozenDom.clone()
    _elementSeq = DOMUtils.getTSpanElements(_dom).toIndexedSeq
  }

  /** Function to get dom externally **/
  final def getDom(): Document = _dom

  /** Function to get all non empty tspan elements externally
    *
    * Each position is a block index
    */
  final def getElements(): Seq[Element] = _elementSeq

  final def mkIndexPair(totalIndex: Int): (Int, Int) = {
    val blockIndex = annotationBlockSeq.indexWhere(b => {
      b.startIndex <= totalIndex && b.nextIndex > totalIndex
    })
    val charIndex = totalIndex - annotationBlockSeq(blockIndex).startIndex
    (blockIndex, charIndex)
  }

  def pair2Total(pair: (Int, Int)): Int = {
    annotationBlockSeq(pair._1).startIndex + pair._2
  }

  /** Function to produce a string representation of an annotation span **/
  private def renderAnnotation(a: AnnotationSpan, length: Int): String = {

    val posi = (0 until length).foldLeft("")((stringAcc, i) => {
      stringAcc + (a.labelMap.get(i) match {
        case Some(B(char)) => char.toLower
        case Some(U(char)) => char.toUpper
        case Some(I) => '~'
        case Some(O) => '-'
        case Some(L) => '$'
        case None => ' '
      })
    })

    val constr =  ", unit: " + {
      val constraintRange = a.annotationTypeSeq(0).constraintRange
      a.annotationTypeSeq.foreach(annoType => {
        assert(annoType.constraintRange == constraintRange, "annotationTypeSeq has inconsistent constraints")
      })
      def loop(cr: ConstraintRange): String = {
        cr match {
          case Single(CharCon) =>
            "char"
          case Single(SegmentCon(annotationTypeName)) =>
            annotationTypeName
          case Range(annotationTypeName, end) =>
            val annotationType = annotationInfoMap(annotationTypeName).annotationType
            val con = annotationType.constraintRange match {
              case Single(c) => c
              case Range(_, c) => c
            }

            if (con == end) {
              annotationTypeName + "." + loop(Single(end))
            } else {
              con match {
                case CharCon =>
                  assert(false)
                  annotationTypeName + "." + loop(Single(con))
                case SegmentCon(_annoTypeName) =>
                  annotationTypeName + "." + loop(Range(_annoTypeName, end))
              }
            }
        }
      }
      loop(constraintRange)
    }

    val annot = {
      "type: " + "{" + a.annotationTypeSeq.map(at => {
        at.name + ": " + at.c
      }).mkString(", ") + "}"
    }

    "| |" + posi + "| " + "{" + annot + constr + "}"

  }

  /** Function to produce a string representation of an annotation block **/
  private def renderAnnotationBlock(bb: AnnotationBlock): String = {
    val next = bb.nextIndex

    val height = (next - 1).toString.size

    val topRulerList = (height to 2 by -1).map(level => {
      "| |"+(bb.startIndex until next).map(i => {
        if (i == bb.startIndex || (i % 10) == 0){
          val divisor = Math.pow(10,level).toInt
          val digit = (i % divisor)/(divisor/10)
          if (digit == 0 && level == height) " " else digit
        } else " "
      }).mkString("")+"|"
    })

    val bottomRuler = "| |" + (bb.startIndex until next).map(_ % 10).mkString("") + "|"

    val ruler = (topRulerList :+ bottomRuler).mkString("\n")

    "\n" + bb.annotationMap.values.toList.reverse.distinct.map(renderAnnotation(_, (next - bb.startIndex))).mkString("\n") + "\n" + ruler + "\n "
  }

  /** Function to create a new annotation block with an additional annotation span **/
  private def addAnnotation(annotationSpan: AnnotationSpan, annotationBlock: AnnotationBlock): AnnotationBlock = {
    require(annotationSpan.labelMap.lastKey < annotationBlock.nextIndex, "annotationSpan is too long for annotationBlock")
    annotationSpan.annotationTypeSeq.foldLeft(annotationBlock)((b, annotationType) => {
      b.copy(annotationMap = b.annotationMap + (annotationType -> annotationSpan))
    })

  }

  /** Sorted set of the index pairs for every character in all the tspans **/
   val totalCharSize: Int = frozenElementSeq.map(_.getText().size).foldLeft(0) {
     case (acc, size) =>  acc + size
   }

  /** Function to return a map of ints to elements based on a range of block indexes
    *
    * the result's keys are block indexes
    */
  final def getElementsInRange(blockIndex1: Int, blockIndex2: Int): IntMap[Element] = {
    require(blockIndex1 <= blockIndex2)
    IntMap((blockIndex1 to blockIndex2).map(blockIndex =>{
      blockIndex -> getElements().toIndexedSeq(blockIndex)
    }): _*)
  }

  /** Function to produce segments
    *
    * It takes an annotation type string, a block index
    * and a char index, a segment of the annotation type that
    * start on or after the provided index pair
    */
  final def getSegment(annotationTypeName: String)(index: Int): Map[Int, Label] = {

    val annotationType = annotationInfoMap(annotationTypeName).annotationType

    def loop(foundFirst: Boolean, blockIndex: Int, charIndex: Int, acc: Map[Int, Map[Int, Label]]): Map[Int, Map[Int, Label]] = {

      if (annotationBlockSeq.size > blockIndex) {
        val block = annotationBlockSeq(blockIndex)
        block.annotationMap.get(annotationType) match {
          case None => loop(foundFirst, blockIndex + 1, 0, acc)
          case Some(annotation) =>
            val labelMap = annotation.labelMap
            labelMap.keys.find(_ >= charIndex) match {
              case None =>
                loop(foundFirst, blockIndex + 1, 0, acc)
              case Some(_charIndex) =>
                val label = labelMap(_charIndex)
                (foundFirst, label) match {
                  case (false, B(char)) if annotationType.c == char => loop(true, blockIndex, _charIndex, acc)
                  case (false, U(char)) if annotationType.c == char => loop(true, blockIndex, _charIndex, acc)
                  case (false, _) => loop(false, blockIndex, _charIndex + 1, acc)
                  case (true, L) =>
                    acc.get(blockIndex) match {
                      case None =>
                        acc + (blockIndex -> IntMap(_charIndex -> L))
                      case Some(rowIntMap) =>
                        acc + (blockIndex -> (rowIntMap + (_charIndex -> L)))
                    }
                  case (true, U(char)) if annotationType.c == char =>
                    acc.get(blockIndex) match {
                      case None =>
                        acc + (blockIndex -> IntMap(_charIndex -> U(char)))
                      case Some(rowIntMap) =>
                        acc + (blockIndex -> (rowIntMap + (_charIndex -> U(char))))
                    }
                  case (true, U(_)) =>
                    loop(foundFirst, blockIndex, _charIndex + 1, acc)
                  case (true, B(char)) if annotationType.c != char =>
                    loop(foundFirst, blockIndex, _charIndex + 1, acc)
                  case (true, label) =>
                    acc.get(blockIndex) match {
                      case None =>
                        loop(foundFirst, blockIndex, _charIndex + 1, acc + (blockIndex -> IntMap(_charIndex -> label)))
                      case Some(rowIntMap) =>
                        loop(foundFirst, blockIndex, _charIndex + 1, acc + (blockIndex -> (rowIntMap + (_charIndex -> label))))
                    }
                }
            }
        }
      } else {
        acc
      }

    }

    val (blockIndex, charIndex) = mkIndexPair(index)
    val table = loop(false, blockIndex, charIndex, Map[Int, Map[Int, Label]]())
    table.flatMap { case (blockIndex, labelMap) =>
      labelMap.map { case (charIndex, char) =>
        pair2Total(blockIndex -> charIndex) -> char
      }
    }

  }

  /** Function to return a map of ints to elements
    *
    * the returned elements correspond to annotations that are of the provided annotation type
    * and start on or after the provided indexes
    */
  final def getElements(annotationTypeName: String)(index: Int): IntMap[Element] = {
    getRangeBySegment(annotationTypeName)(getSegment(annotationTypeName)(index)) match {
      case None =>
        IntMap[Element]()
      case Some((startIndex, endIndex)) =>
        val (blockBIndex, _) = mkIndexPair(startIndex)
        val (blockLIndex, _) = mkIndexPair(endIndex)
        getElementsInRange(blockBIndex, blockLIndex)
    }
  }


  /** Function to return a sorted set of b-index pairs given a constraint range **/
  final def getBIndexSet(constraintRange: ConstraintRange): SortedSet[Int] = {
    constraintRange match {
      case Single(CharCon) =>
        SortedSet((0 until totalCharSize):_*)
      case Single(SegmentCon(annotationTypeName)) =>
        annotationInfoMap.get(annotationTypeName) match {
          case Some(annotationInfo) => annotationInfo.bIndexSortedSet
          case None => SortedSet()
        }
      case Range(annotationTypeName, endCon) =>
        def loop(bIndexSortedSetAcc: SortedSet[Int], constraint: Constraint): SortedSet[Int] = {
          (constraint, endCon) match {
            case (CharCon, SegmentCon(_)) =>
              require(false, "constraintRange's end does not follow from its start")
              SortedSet[Int]()
            case (x, y) if (x == y) =>
              bIndexSortedSetAcc
            case (SegmentCon(annotationTypeName), _) =>

              val _bIndexSortedSetAcc = bIndexSortedSetAcc.flatMap(bIndex => {
                getSegment(annotationTypeName)(bIndex).keySet
              })

              val annotationType = annotationInfoMap(annotationTypeName).annotationType
              val _constraint = annotationType.constraintRange match {
                case Single(c) => c
                case Range(_, c) => c
              }

              loop(_bIndexSortedSetAcc, _constraint)
          }
        }

        annotationInfoMap.get(annotationTypeName) match {
          case Some(annotationInfo) =>
            loop(annotationInfo.bIndexSortedSet, SegmentCon(annotationTypeName))
          case None =>
            SortedSet()
        }
      case _ =>
        require(false, "constraintRange is illformed")
        SortedSet[Int]()
    }
  }


  final def getBIndexSetByAnnotationType(annoType: String) = {
    getBIndexSet(Single(SegmentCon(annoType)))
  }

  def getBIndexSetWithinRange(annoType: String)(range: (Int, Int)) = {
    getBIndexSetByAnnotationType(annoType).filter(i => i >= range._1 && i <= range._2)
  }

  final def getFilteredBIndexSet(filterType: String, annoType: String) = {
    getBIndexSet(Range(filterType, SegmentCon(annoType)))
  }

  def getFilteredBIndexSetWithinRange(filterType: String, annoType: String)(range: (Int, Int)) = {
    getFilteredBIndexSet(filterType, annoType).filter(i => i >= range._1 && i <= range._2)
  }


  def getSegmentSet(annotationTypeName: String) = {
    getBIndexSetByAnnotationType(annotationTypeName).map(i => getSegment(annotationTypeName)(i))
  }

  def getFilteredSegmentSet(filterType: String, annotationTypeName: String) = {
    getFilteredBIndexSet(filterType, annotationTypeName).map(i => getSegment(annotationTypeName)(i))
  }

  def getRangeBySegment(annotationTypeName: String)(segment: Map[Int, Label]): Option[(Int, Int)] = {
    if (segment.isEmpty) {
      None
    } else {
      def findLastIndex(startIndex: Int, constraint: Constraint): Int = {
        constraint match {
          case CharCon =>
            startIndex
          case SegmentCon(annoTypeName) =>
            val annoType = annotationInfoMap(annoTypeName).annotationType
            val segment =  getSegment(annoTypeName)(startIndex)
            val lastIndex = segment.keySet.max
            val con = annoType.constraintRange match {
              case Single(c) => c
              case Range(_, c) => c
            }
            findLastIndex(lastIndex, con)
        }
      }

      val firstIndex = segment.keySet.min
      val con = annotationInfoMap(annotationTypeName).annotationType.constraintRange match {
        case Single(c) => c
        case Range(_, c) => c
      }
      val lastIndex = findLastIndex(segment.keySet.max, con)

      Some((firstIndex, lastIndex))
    }
  }

  def getRangeSet(annotationType: String): SortedSet[(Int, Int)] = {
    getBIndexSetByAnnotationType(annotationType).flatMap(i => {
      getRange(annotationType)(i)
    })
  }

  /** Function to return a map of ints to int string pairs based on a index range
    *
    * the returned map's keys are block indexes, and each corresponding value is a char index and
    * the text that exists in that block starting from that char index
    */
  final def getTextByRange(range: (Int, Int)): String = {

    val index1 = range._1
    val index2 = range._2

    val (blockIndex1, charIndex1) = mkIndexPair(index1)
    val (blockIndex2, charIndex2) = mkIndexPair(index2)

    getElementsInRange(blockIndex1, blockIndex2).map { case (blockIndex, e) =>

      if (blockIndex == blockIndex1 && blockIndex == blockIndex2) {
        e.getText().take(charIndex2 + 1).drop(charIndex1)

      } else if (blockIndex == blockIndex1) {
        e.getText().drop(charIndex1)

      } else if (blockIndex == blockIndex2) {
        e.getText().take(charIndex2 + 1)

      } else {
        e.getText()

      }

    } mkString("")
  }


  def getTextOption(annoTypeString: String)(index: Int): Option[(Int, String)] = {
    getRangeBySegment(annoTypeString)(getSegment(annoTypeString)(index)) map {
      case range =>
        val (startIndex, endIndex) = range
        (startIndex, getTextByRange(range))
    }
  }

  def getTextSeq(annoTypeString: String): Seq[(Int, String)] = {
    getBIndexSetByAnnotationType(annoTypeString).toSeq.flatMap(i => getTextOption(annoTypeString)(i))
  }

  def getFilteredTextSeq(filterType: String, annoTypeString: String): Seq[(Int, String)] = {
    getFilteredBIndexSet(filterType, annoTypeString).toSeq.flatMap(i => getTextOption(annoTypeString)(i))
  }

  def getAnnotationByTypeString(annoTypeString: String): Annotation = {
    annotationInfoMap.get(annoTypeString) match {
      case Some(annotationInfo) =>
        val bIndexSet = annotationInfo.bIndexSortedSet
        getAnnotation(bIndexSet, annoTypeString)
      case None =>
        Annotation(AnnotationType(annoTypeString, 'a', Single(CharCon)), LeafContent(List()))
    }
  }

  def getRange(annoType: String)(index: Int): Option[(Int, Int)] = {
    getRangeBySegment(annoType)(getSegment(annoType)(index))
  }

  def getAnnotation(bIndexSet: SortedSet[Int], annoTypeString: String): Annotation = {
    val annotationInfo = annotationInfoMap(annoTypeString)
    require(bIndexSet.subsetOf(annotationInfo.bIndexSortedSet))

    val annoType = annotationInfo.annotationType
    val content = annoType.constraintRange match {
      case Single(CharCon) =>
        LeafContent(bIndexSet.toList.flatMap(index => {
          getTextOption(annoTypeString)(index)
        }))
      case Range(_, CharCon) =>
        LeafContent(bIndexSet.toList.flatMap(index => {
          getTextOption(annoTypeString)(index)
        }))
      case Single(SegmentCon(nextAnnoTypeString)) =>
        InnerContent(bIndexSet.toList.map(index => {
          val segmentBIndexSet = SortedSet[Int]() ++ getSegment(annoTypeString)(index).keySet
          getAnnotation(segmentBIndexSet, nextAnnoTypeString)
        }))
      case Range(_, SegmentCon(nextAnnoTypeString)) =>
        InnerContent(bIndexSet.toList.map(index => {
          val segmentBIndexSet = SortedSet[Int]() ++ getSegment(annoTypeString)(index).keySet
          getAnnotation(segmentBIndexSet, nextAnnoTypeString)
        }))
    }

    Annotation(annoType, content)

  }

  /** Function to produce a new Annotator with additional annotations
    *
    * nameCharPairSeq is a sequence of new annotation type strings and corresponding chars
    * constraintRange specifies the the parts of text the annotations are restricted to
    * fullLabelMap specifies where to add annotation labels
    *
    * labels with index pairs that are outside of the annotatable region (defined by the dom and constraintRange)
    * will not be added
    */
  final def annotateWithIndexPairMap(
      nameCharPairSeq: Seq[(String, Char)],
      constraintRange: ConstraintRange,
      fullLabelMap: Map[(Int, Int), Label]
  ): Annotator = {

    val annotatableIndexSet = getBIndexSet(constraintRange)
    if (annotatableIndexSet.isEmpty) {
      this
    } else {
      val annotationTypeSeq = nameCharPairSeq.map {
        case (name, char) =>
          assert(!annotationInfoMap.contains(name), "annotation type named " + name + " already exists")
          AnnotationType(name, char, constraintRange)
      }

      val labelTable = fullLabelMap.filter(p => {
        val indexPair = p._1
        annotatableIndexSet.contains(pair2Total(indexPair))
      }).foldLeft(IntMap[IntMap[Label]]()) {
        case (tableAcc, ((blockIndex, charIndex), label)) =>
          if (tableAcc.contains(blockIndex)) {
            tableAcc + (blockIndex -> (tableAcc(blockIndex) + (charIndex -> label)))
          } else {
            tableAcc + (blockIndex -> IntMap(charIndex -> label))
          }
      }

      val _annotationBlockSeq = annotationBlockSeq.zipWithIndex.map { case (block, blockIndex) => {
        labelTable.get(blockIndex) match {
          case None => block
          case Some(labelMap) =>
            val annotation = AnnotationSpan(labelMap, annotationTypeSeq)
            addAnnotation(annotation, block)
        }
      }}

      val _annotationInfoMap =  {
        val annotationInfoList = annotationTypeSeq.map {
          case _annotationType =>
            val char = _annotationType.c
            val bIndexSet = annotatableIndexSet.filter(index => {
              val (blockIndex, charIndex) = mkIndexPair(index)
              labelTable.contains(blockIndex) && ({
                val labelMap = labelTable(blockIndex)
                labelMap.contains(charIndex) && ({
                  val label = labelMap(charIndex)
                  label == U(char) || label == B(char)
                })
              })
            })

            _annotationType.name -> AnnotationInfo(_annotationType, bIndexSet)

        }

        annotationInfoMap ++ annotationInfoList
      }

      new Annotator(
        frozenDom,
        _annotationBlockSeq,
        _annotationInfoMap,
        annotationLinkSet
      )
    }


  }

  final def annotate(
    nameCharPairSeq: Seq[(String, Char)],
    constraintRange: ConstraintRange,
    fullLabelMap: Map[Int, Label]
  ): Annotator = {
    annotateWithIndexPairMap(nameCharPairSeq, constraintRange, fullLabelMap.map(p => {
      val (index, label) = p
      mkIndexPair(index) -> label
    }))
  }

  /** Function to return a new Annotator that that has the provided links added **/
  final def annotateLink(_annotationLinkSet: Set[AnnotationLink]): Annotator = {

    val bIndexSetMap = _annotationLinkSet.flatMap(_.attrValueMap.values).map(v => {
      val (annoTypeStr, _) = v
      annoTypeStr -> getBIndexSet(Single(SegmentCon(annoTypeStr)))
    }).toMap

    new Annotator(
      frozenDom, annotationBlockSeq, annotationInfoMap,
      annotationLinkSet ++ _annotationLinkSet.filter(annoLink => {
        annoLink.attrValueMap.foldLeft(true) {
          case (boolAcc, (attr, (annoTypeStr, index))) =>
            boolAcc && bIndexSetMap.contains(annoTypeStr) && bIndexSetMap(annoTypeStr).contains(index)
        }
      })
    )


  }

  private val xmlOutputProcessor = new AbstractXMLOutputProcessor {
    override def write(writer: Writer, str: String) = {
      super.write(
          writer,
          if (str == null) {
            str
          } else {
            str.replaceAll("&#xA;", "\n").replaceAll("<svg:tspan", "\n<svg:tspan")
          }
      )
    }
  }

  final def mkAnnotatedDom(): Document = {
    val writableDom = frozenDom.clone()
    DOMUtils.getTSpanElements(writableDom).zipWithIndex.foreach { case (e, i) => {
      val block = annotationBlockSeq(i)
      e.setAttribute("bio", renderAnnotationBlock(block))
    }}

    val root = writableDom.getRootElement()
    val annotationLinksE = new Element("annotation-links")
    annotationLinkSet.map(link => {
      val e = new Element(link.name)

      link.attrValueMap.foreach(pair => {
        val (attr, (typeString, totalIndex)) = pair
        e.setAttribute(attr, typeString + " " + totalIndex.toString)
      })
      annotationLinksE.addContent(e)
    })
    root.addContent(annotationLinksE)
    writableDom
  }


  /** Function to write a string representation of the Annotator instance to the provided file path**/
  final def write(filePath: String): Annotator = {
    val writableDom = mkAnnotatedDom()

    //format
    val outputter = new XMLOutputter(Format.getPrettyFormat(), xmlOutputProcessor)

    //write
    val out = new FileOutputStream(filePath)
    outputter.output(writableDom, out)
    this

  }

}
