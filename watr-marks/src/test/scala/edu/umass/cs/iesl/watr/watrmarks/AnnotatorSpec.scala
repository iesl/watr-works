package edu.umass.cs.iesl.watr.watrmarks

import org.scalatest._

import scala.collection.immutable.IntMap
import scala.collection.immutable.ListMap

import org.jdom2.filter.ElementFilter
import org.jdom2.Element
import org.jdom2.Document
import org.jdom2.util.IteratorIterable
import org.jdom2.output.Format
import org.jdom2.output.XMLOutputter

import org.xml.sax.InputSource
import org.xml.sax.XMLReader

class AnnotatorSpec extends FlatSpec {

  val dom = new Document()

  val e = {
    // <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
    //   <g transform="matrix(1 0 0 -1 0 792)">
    //     <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
    //       <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
    //     </text>
    //     <text transform="translate(136.8 669.12) scale(1, -1)">
    //       <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
    //       <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
    //     </text>
    //   </g>
    // </svg>
    val _e = new Element("svg")
      .setAttribute("version", "1.1")
      .setAttribute("width", "612px")
      .setAttribute("height", "3168px")
      .setAttribute("viewBox", "0 0 612 3168")
     dom.setRootElement(_e)
     _e
  }

  val e_1 = {
    // <g transform="matrix(1 0 0 -1 0 792)">
    //   <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
    //     <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
    //   </text>
    //   <text transform="translate(136.8 669.12) scale(1, -1)">
    //     <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
    //     <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
    //   </text>
    // </g>
    val _e = new Element("g")
      .setAttribute("transform", "matrix(1 0 0 -1 0 792)")
    e.addContent(_e)
    _e
  }

  val e_1_1 = {
    // <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
    //   <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
    // </text>
    val _e = new Element("text")
      .setAttribute("transform", "matrix(0 1 -1 0 32 256) scale(1, -1)")
    e_1.addContent(_e)
    _e
  }

  val e_1_1_1 = {
    // <tspan x="0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1" endX="100.2" y="0" font-size="20px">abcdefghijkl</tspan>
    val _e = new Element("tspan")
      .setAttribute("x", "0 8.88 15.54 29.98 35.54 45.54 51.1 61.1 71.1 81.1 91.1 96.1")
      .setAttribute("endX", "100.2")
      .setAttribute("y", "0")
      .setAttribute("font-size", "20px")
      .setText("abcdefghijkl")
    e_1_1.addContent(_e)
    _e
  }

  val e_1_2 = {
    // <text transform="translate(136.8 669.12) scale(1, -1)">
    //   <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
    //   <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
    // </text>
    val _e = new Element("text")
      .setAttribute("transform", "translate(136.8 669.12) scale(1, -1)")
    e_1.addContent(_e)
    _e
  }

  val e_1_2_1 = {
    // <tspan x="0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83" endX="112.43" y="0" font-size="17.2154px">mnopqrstuvwxyz1</tspan>
    val _e = new Element("tspan")
      .setAttribute("x", "0 11.51 20.15 25.91 33.59 43.19 48.93 53.03 65.51 73.19 77.99 86.63 92.39 97.19 105.83")
      .setAttribute("endX", "112.43")
      .setAttribute("y", "0")
      .setAttribute("font-size", "17.2154px")
      .setText("mnopqrstuvwxyz1")
    e_1_2.addContent(_e)
    _e
  }

  val e_1_2_2 = {
    // <tspan x="137.16 146.75 154.43 164.03 171.71 186.11" endX="191.22" y="19.91" font-size="17.2154px">234567</tspan>
    val _e = new Element("tspan")
      .setAttribute("x", "137.16 146.75 154.43 164.03 171.71 186.11")
      .setAttribute("endX", "191.22")
      .setAttribute("y", "19.91")
      .setAttribute("font-size", "17.2154px")
      .setText("234567")
    e_1_2.addContent(_e)
    _e
  }

  val outputter = new XMLOutputter(Format.getPrettyFormat())
  // println("e\n"+ outputter.outputString(e))
  // println("e_1\n"+ outputter.outputString(e_1))
  // println("e_1_1\n"+ outputter.outputString(e_1_1))
  // println("e_1_1_1\n"+ outputter.outputString(e_1_1_1))
  // println("e_1_2\n"+ outputter.outputString(e_1_2))
  // println("e_1_2_1\n"+ outputter.outputString(e_1_2_1))
  // println("e_1_2_2\n"+ outputter.outputString(e_1_2_2))

  "mkIndexPair and pair2Total" should "be inverses" in {
    assertResult((2,3)) {
      annotator.mkIndexPair(annotator.pair2Total((2,3)))
    }

    assertResult(15) {
     annotator.pair2Total( annotator.mkIndexPair(15))
    }
  }

  "fontSize" should "raise an exception if the provided element has no attribute font-size" in {
    // val _ =... used to silence compiler warnings for discarding unused value
    val _ = intercept[NullPointerException] {
      Attr.fontSize(e)
    }
  }

  it should "return a double if the element has attribute font-size with value \"[some numer]px\"" in {
    assertResult(20) {
      Attr.fontSize(e_1_1_1)
    }

    assertResult(17.2154) {
      Attr.fontSize(e_1_2_1)
    }
  }

  "y" should "raise an exception if the provided element has no attribute y" in {
    val _ = intercept[NullPointerException] {
      Attr.y(e_1_1)
    }
  }

  it should "return a double if the element has attribute y with a numeric value" in {
    assertResult(0) {
      Attr.y(e_1_1_1)
    }

    assertResult(19.91) {
      Attr.y(e_1_2_2)
    }

  }

  "xs" should  "raise an exception if the provided element has no attribute x" in {
    val _ = intercept[NullPointerException] {
      Attr.y(e_1_1)
    }
  }

  it should "return a list of doubles if the element has attribute x with the value as a space separated list of numbers" in {
    assertResult(List(0, 8.88, 15.54, 29.98, 35.54, 45.54, 51.1, 61.1, 71.1, 81.1, 91.1, 96.1)) {
      Attr.xs(e_1_1_1)
    }
  }

  "commonAncestor" should "raise an exception if either argument is null" in {
    intercept[IllegalArgumentException] {
      DOMUtils.commonAncestor(null, null)
    }

    intercept[IllegalArgumentException] {
      DOMUtils.commonAncestor(null, e_1)
    }

    val _ = intercept[IllegalArgumentException] {
      DOMUtils.commonAncestor(e_1, null)
    }
  }

  it should "raise an exception if arguments are in different trees" in {
    val _ = intercept[IllegalArgumentException] {
      DOMUtils.commonAncestor(new Element("a"), new Element("b"))
    }
  }

  it should "find ancestor if elements have different depths in the same tree" in {
    assertResult(e_1) {
      DOMUtils.commonAncestor(e_1_1, e_1_2_1)
    }
  }

  it should "return the most recent common ancestor" in {
    assertResult(e_1_2) {
      DOMUtils.commonAncestor(e_1_2_1, e_1_2_2)
    }

    assertResult(e_1) {
      DOMUtils.commonAncestor(e_1_1_1, e_1_2_1)
    }

  }

  "getTransformedCoords" should "raise an exception if the first argument is missing attributes y, endX, or x" in {
    val _ = intercept[NullPointerException] {
      Annotator.getTransformedCoords(e_1_1, e)
    }
  }

  it should "raise an exception if second argument is not an ancestor of the first" in {
    val _ = intercept[IllegalArgumentException] {
      Annotator.getTransformedCoords(e_1_2_1, e_1_1)
    }
  }

  it should """return a PositionGroup if provided source element has attributes y, endX and x
               and the second argument is an ancestor of the first""" in {

    assertResult(
      Annotator.PositionGroup(
        List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
        32.0,
        List(536.0, 527.12, 520.46, 506.02, 500.46, 490.46, 484.9, 474.9, 464.9, 454.9, 444.9, 439.9)
      )
    ) {
      Annotator.getTransformedCoords(e_1_1_1, e)
    }

    assertResult(
      Annotator.PositionGroup(
        List(32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0, 32.0),
        32.0,
        List(256.0, 264.88, 271.54, 285.98, 291.54, 301.54, 307.1, 317.1, 327.1, 337.1, 347.1, 352.1)
      )
    ) {
      Annotator.getTransformedCoords(e_1_1_1, e_1_1)
    }

    assertResult(
      Annotator.PositionGroup(
        List(137.16, 146.75, 154.43, 164.03, 171.71, 186.11),
        191.22,
        List(19.91, 19.91, 19.91, 19.91, 19.91, 19.91)
      )
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e_1_2_2)
    }

    assertResult(
      Annotator.PositionGroup(
        List(273.96000000000004, 283.55, 291.23, 300.83000000000004, 308.51, 322.91),
        328.02,
        List(142.79, 142.79, 142.79, 142.79, 142.79, 142.79)
      )
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e)
    }
  }

  it should "not make transformations for ancestral elements lacking transform attributes" in {
    assertResult(
      Annotator.getTransformedCoords(e_1_2_2, e)
    ) {
      Annotator.getTransformedCoords(e_1_2_2, e_1)
    }
  }

  //"mkIndexPairSeq" should "return a sequence of int pairs" in {
  //  assertResult(
  //    IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
  //  ) {
  //    val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
  //    Annotator.mkIndexPairSeq(textMap)
  //  }
  //}

  //"mkIndexPairMap" should "return a map of ints to int pairs, when the first argument is an int pair seq" in {

  //  assertResult(
  //    IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3))
  //  ) {
  //    val indexPairSeq = IndexedSeq((3,4), (3,5), (3,6), (3,7), (3,8), (5,0), (5,1), (5,2), (5,3))
  //    val bIndexPairSet = Set((3,6), (5,1))
  //    Annotator.mkIndexPairMap(indexPairSeq, bIndexPairSet)
  //  }
  //}

  //it should "return a map of ints to int pairs, when first argument is a text map" in {
  //  assertResult(
  //    IntMap(0 -> (3,4), 1 -> (3,5), 3 -> (3,6), 4 -> (3,7), 5 -> (3,8), 6 -> (5,0), 8 -> (5,1), 9 -> (5,2), 10 -> (5,3))
  //  ) {
  //    val textMap = IntMap(3 -> (4, "abcde"), 5 -> (0, "fghi"))
  //    val bIndexPairSet = Set((3,6), (5,1))
  //    Annotator.mkIndexPairMap(textMap, bIndexPairSet)
  //  }
  //}

  "mkTextWithBreaks" should "return a string of text with extra characters inserted" in {
    assertResult("abcde fghi") {
      Annotator.mkTextWithBreaks("abcdefghi", Set(5), ' ')
    }
  }

  //Annotator Instances

  import Annotator._
  val annotator = Annotator(dom)

  val quailTable = Map(
    0 -> B('q'), 1 -> I, 2 -> I, 3 -> L,
    4 -> B('q'), 5 -> I, 6 -> I, 7 -> L,
    8 -> B('q'), 9 -> I, 10 -> I, 11 -> L,
    12 -> B('q'), 13 -> I, 14 -> I, 15 -> L,
    16 -> B('q'), 17 -> I, 18 -> I, 19 -> L,
    20 -> B('q'), 21 -> I, 22 -> I, 23 -> L,
    24 -> B('q'), 25 -> I, 26 -> I, 27 -> L,
    28 -> B('q'), 29 -> I, 30 -> I, 31 -> L,
    32 -> U('q')
  )

  val annotator2 = annotator.annotate(List("quail" -> 'q'), Single(CharCon), quailTable)
  // bio="
  //   | |q~~$q~~$q~~$| {type: {quail: q}, unit: char}
  //   | |          1 |
  //   | |012345678901|
  //    ">abcdefghijkl</tspan>
  // bio="
  //   | |q~~$q~~$q~~$q~~| {type: {quail: q}, unit: char}
  //   | |1       2      |
  //   | |234567890123456|
  //    ">mnopqrstuvwxyz1</tspan>
  // bio="
  //   | |$q~~$Q| {type: {quail: q}, unit: char}
  //   | |2  3  |
  //   | |789012|
  //    ">234567</tspan>


  val falconTable = Map(0 -> B('f'), 8 -> L, 12 -> B('f'), 20 -> L)

  val annotator3 = annotator2.annotate(List("falcon" -> 'f'), Single(SegmentCon("quail")), falconTable)
  // bio="
  //   | |f       $   | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$| {type: {quail: q}, unit: char}
  //   | |          1 |
  //   | |012345678901|
  //    ">abcdefghijkl</tspan>
  // bio="
  //   | |f       $      | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$q~~| {type: {quail: q}, unit: char}
  //   | |1       2      |
  //   | |234567890123456|
  //    ">mnopqrstuvwxyz1</tspan>
  // bio="
  //   | |$q~~$Q| {type: {quail: q}, unit: char}
  //   | |2  3  |
  //   | |789012|
  //    ">234567</tspan>



  val penguinTable = Map(0 -> U('p'), 12 -> U('p'), 24 -> U('p'))

  val annotator4 = annotator3.annotate(List("penguin" -> 'p'), Single(SegmentCon("quail")), penguinTable)
  // bio="
  //   | |P           | {type: {penguin: p}, unit: quail}
  //   | |f       $   | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$| {type: {quail: q}, unit: char}
  //   | |          1 |
  //   | |012345678901|
  //    ">abcdefghijkl</tspan>
  // bio="
  //   | |P           P  | {type: {penguin: p}, unit: quail}
  //   | |f       $      | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$q~~| {type: {quail: q}, unit: char}
  //   | |1       2      |
  //   | |234567890123456|
  //    ">mnopqrstuvwxyz1</tspan>
  // bio="
  //   | |$q~~$Q| {type: {quail: q}, unit: char}
  //   | |2  3  |
  //   | |789012|
  //    ">234567</tspan>


  val annoWithLinks = annotator4.annotateLink(Set(
    AnnotationLink("employment", Map(("employer", ("penguin", 12)), ("employee", ("quail", 4)))),
    AnnotationLink("employment", Map(("employer", ("penguin", 24)), ("employee", ("quail", 28)))),
    AnnotationLink("employment", Map(("employer", ("penguin", 24)), ("employee", ("quail", 32))))
  ))
  // bio="
  //   | |P           | {type: {penguin: p}, unit: quail}
  //   | |f       $   | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$| {type: {quail: q}, unit: char}
  //   | |          1 |
  //   | |012345678901|
  //    ">abcdefghijkl</tspan>
  //   bio="
  //   | |P           P  | {type: {penguin: p}, unit: quail}
  //   | |f       $      | {type: {falcon: f}, unit: quail}
  //   | |q~~$q~~$q~~$q~~| {type: {quail: q}, unit: char}
  //   | |1       2      |
  //   | |234567890123456|
  //    ">mnopqrstuvwxyz1</tspan>
  //   bio="
  //   | |$q~~$Q| {type: {quail: q}, unit: char}
  //   | |2  3  |
  //   | |789012|
  //    ">234567</tspan>
  //   <annotation-links>
  //     <employment employer="penguin 24" employee="quail 32" />
  //     <employment employer="penguin 24" employee="quail 28" />
  //     <employment employer="penguin 12" employee="quail 4" />
  //   </annotation-links>

  // println("4\n"+
  //   outputter.outputString(
  //     annoWithLinks.mkAnnotatedDom()
  //   )
  // )

  "Annotator()" should "create an Annotator instance with annotationBlockSeq populated without any annotations" in {

    val emptyMap = ListMap[AnnotationType, AnnotationSpan]()
    val expectedResults = IndexedSeq(
        AnnotationBlock(0,12, emptyMap),
        AnnotationBlock(12,27, emptyMap),
        AnnotationBlock(27,33, emptyMap)
   )

    (0 to 2).foreach(i => {
      val expected = expectedResults(i)
      val actual =  annotator.annotationBlockSeq(i)
      assertResult(expected)(actual)
    })

  }

  it should "create an annotator such that the next index minus the start index of each annotation block\n" +
  "equals the text length of the corresponding element produced by getElements() " in {

    val es = annotator.getElements()

    es.toIndexedSeq.zipWithIndex.foreach {
      case (e, i) =>
        val annoBlock = annotator.annotationBlockSeq(i)
        val expected = e.getText().size
        val actual = annoBlock.nextIndex - annoBlock.startIndex
        assertResult(expected)(actual)
    }

  }

  it should "load annotations already in the dom attribute when called with the load option" in {
    val domWithAnnotations = annoWithLinks.mkAnnotatedDom()
    val anno = Annotator(domWithAnnotations, true)
    assertResult(annoWithLinks.annotationInfoMap)(anno.annotationInfoMap)
    assertResult(annoWithLinks.annotationBlockSeq)(anno.annotationBlockSeq)
    assertResult(annoWithLinks.annotationLinkSet)(anno.annotationLinkSet)
  }

  "annotate" should "raise an exception if called with an annotation type that already exists" in {
    val table = annotator.getBIndexSet(Single(CharCon)).map(_ -> U('q')).toMap
    val _ = intercept[AssertionError] {
      annotator4.annotate(List("quail" -> 'q'), Single(CharCon), table)
    }
  }

  it should "return annotator unchanged if called with a constraint range containing an annotation\n" +
  "type that does not exist in the annotator" in {

    val newA = annotator.annotate(List("quail" -> 'q'), Single(SegmentCon("xyz")), quailTable)
    assertResult(annotator.annotationBlockSeq) { newA.annotationBlockSeq }
    assertResult(annotator.annotationInfoMap) { newA.annotationInfoMap }

  }


  it should "raise an exception if called with a constraint range where\n" +
  "the range's annotation type does not descend from the constraint" in {
    val table = annotator.getBIndexSet(Single(CharCon)).map(_ -> U('x')).toMap
    val _ = intercept[IllegalArgumentException] {
      annotator4.annotate(List("xyz" -> 'x'), Range("falcon", SegmentCon("penguin")), table)
    }
  }


  it should "create an Annotator instance with annotationBlockSeq populated with some annotations" in {

    val expected = {
      AnnotationBlock(
          0,12,
          ListMap(
              AnnotationType("quail", 'q', Single(CharCon)) ->
              AnnotationSpan(
                  IntMap(
                      0 -> B('q'), 1 -> I, 2 -> I, 3 -> L,
                      4 -> B('q'), 5 -> I, 6 -> I, 7 -> L,
                      8 -> B('q'), 9 -> I, 10 -> I, 11 -> L
                  ),
                  Seq(AnnotationType("quail", 'q', Single(CharCon)))
              )
          )
      )
    }

    val actual = annotator2.annotationBlockSeq(0)
    assertResult(expected)(actual)
  }


  "annotateLink" should "create a new annotator containing links" in {

    assertResult(Set())(annotator4.annotationLinkSet)
    assertResult(Set(
      AnnotationLink("employment", Map(("employer", ("penguin", 12)), ("employee", ("quail", 4)))),
      AnnotationLink("employment", Map(("employer", ("penguin", 24)), ("employee", ("quail", 28)))),
      AnnotationLink("employment", Map(("employer", ("penguin", 24)), ("employee", ("quail", 32))))
    ))(annoWithLinks.annotationLinkSet)

  }


  "getBIndexSet" should "return empty set if the constraint range specifies a non existent annotation type" in {
    assertResult(Set()) {
      annotator.getBIndexSet(Range("xyz", CharCon))
    }
  }

  it should "raise an exception if the range's annotation type does not descend from the constraint" in {

    val _ = intercept[IllegalArgumentException] {
      annotator4.getBIndexSet(Range("falcon", SegmentCon("penguin")))
    }
  }

  it should "produce all index pairs if the constraint is CharCon" in {

    assertResult(Set(
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
      13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
      23, 24, 25, 26, 27, 28, 29, 30, 31, 32
    )) {
      annotator.getBIndexSet(Single(CharCon))
    }
  }

  it should "produce index pairs on a single of segment constraint of existing annotation type" in {

    assertResult(Set(0, 4, 8, 12, 16, 20, 24, 28, 32)) {
      annotator2.getBIndexSet(Single(SegmentCon("quail")))
    }
  }

  it should "produce index pairs on a range where the annotation type descends from the constraint" in {

    assertResult(Set(0, 12)) {
      annotator3.getBIndexSet(Single(SegmentCon("falcon")))
    }

    assertResult(Set(0, 8, 12, 20)) {
      annotator3.getBIndexSet(Range("falcon", SegmentCon("quail")))
    }

    assertResult(Set(
        0, 1, 2, 3, 8, 9, 10, 11,
        12, 13, 14, 15, 20, 21, 22, 23
    )) {
      annotator3.getBIndexSet(Range("falcon", CharCon))
    }

    assertResult(Set(0, 12, 24)) {
      annotator4.getBIndexSet(Single(SegmentCon("penguin")))
    }
  }

  "getSegment" should "raise an exception if the specified annotation type does not exist" in {
    val _ = intercept[NoSuchElementException] {
      annotator4.getSegment("xyz")(0)
    }
  }

  it should "produce segments of the provided annotation type that start on or after\n" +
  "the provided index pair" in {

    assertResult(Map(0 -> B('q'), 1 -> I, 2 -> I, 3 -> L)) {
      annotator4.getSegment("quail")(0)
    }

    assertResult(Map(4 -> B('q'), 5 -> I, 6 -> I, 7 -> L)) {
      annotator4.getSegment("quail")(2)
    }

    assertResult(Map(4 -> B('q'), 5 -> I, 6 -> I, 7 -> L)) {
      annotator4.getSegment("quail")(4)
    }

    assertResult(Map(12 -> B('f'), 20 -> L)) {
      annotator4.getSegment("falcon")(12)
    }

    assertResult(Map(12 -> U('p'))) {
      annotator4.getSegment("penguin")(2)
    }

  }

  "getRange" should "characters that make up an annotation whose B or U starts on or after the provided index pair\n" +
  "to the U or L of the annotation type that the provided annotation type ultimately descends from" in {

    assertResult(Some((0,3))) {
      annotator4.getRange("quail")(0)
    }

    assertResult(Some((4,7))) {
      annotator4.getRange("quail")(2)
    }

    assertResult(Some((4,7))) {
      annotator4.getRange("quail")(4)
    }

    assertResult(Some((12,23))) {
      annotator4.getRange("falcon")(12)
    }

    assertResult(Some((12, 15))) {
      annotator4.getRange("penguin")(2)
    }
  }

  "getElementsInRange" should "raise an exception if the provided block index does not exist" in {
    intercept[IndexOutOfBoundsException] {
      annotator4.getElementsInRange(1, 5)
    }

    val _ = intercept[IndexOutOfBoundsException] {
      annotator4.getElementsInRange(-2, 1)
    }

  }

  it should "raise an exception if the second block index is less than the first" in {
    val _ = intercept[IllegalArgumentException] {
      annotator4.getElementsInRange(2, 1).mapValues(_.getText())
    }
  }

  it should "produce a map of block indexes to elements containing the elements\n" +
  "that fall in the provided range of block indexes" in {

    assertResult(IntMap(0 -> "abcdefghijkl")) {
      annotator4.getElementsInRange(0, 0).mapValues(_.getText())
    }

    assertResult(IntMap(0 -> "abcdefghijkl", 1 -> "mnopqrstuvwxyz1", 2 -> "234567")) {
      annotator4.getElementsInRange(0, 2).mapValues(_.getText())
    }

    assertResult(IntMap(1 -> "mnopqrstuvwxyz1", 2 -> "234567")) {
      annotator4.getElementsInRange(1, 2).mapValues(_.getText())
    }

  }

  "getElements" should "produce elements that have text between index pairs marked as B and L\n" +
  "or as U of the provided annotation on or after the provided block and char index" in {
    assertResult(IntMap(0 -> "abcdefghijkl")) {
      annotator4.getElements("quail")(0).mapValues(_.getText())
    }

    assertResult(IntMap(0 -> "abcdefghijkl")) {
      annotator4.getElements("quail")(2).mapValues(_.getText())
    }

    assertResult(IntMap(0 -> "abcdefghijkl")) {
      annotator4.getElements("quail")(4).mapValues(_.getText())
    }

    assertResult(IntMap(1 -> "mnopqrstuvwxyz1")) {
      annotator4.getElements("falcon")(12).mapValues(_.getText())
    }

    assertResult(IntMap(1 -> "mnopqrstuvwxyz1")) {
      annotator4.getElements("penguin")(2).mapValues(_.getText())
    }
  }

  "getTextByRange" should "produce a map of block indexes to pairs of char index with text pair\n" +
  "where the char index is that of the first char in the associated text and all the texts exist in the provided range" in {

    assertResult("abcd") {
      annotator4.getTextByRange((0,3))
    }

    assertResult("efgh") {
      annotator4.getTextByRange((4,7))
    }

    assertResult("mnopqrstuvwx") {
      annotator4.getTextByRange((12, 23))
    }

    assertResult("mnop") {
      annotator4.getTextByRange((12, 15))
    }

    assertResult("stuvwxyz12345") {
      annotator4.getTextByRange((18, 30))
    }

  }


  "getText" should "produce a text map of provided annotation type and begins\n" +
  "on or after the provided indexes" in {
    assertResult(Some((0, "abcd"))) {
      annotator4.getTextOption("quail")(0)
    }

    assertResult(Some((4, "efgh"))) {
      annotator4.getTextOption("quail")(2)
    }

    assertResult(Some((4, "efgh"))) {
      annotator4.getTextOption("quail")(4)
    }

    assertResult(Some((12, "mnopqrstuvwx"))) {
      annotator4.getTextOption("falcon")(12)
    }

    assertResult(Some((12, "mnop"))) {
      annotator4.getTextOption("penguin")(2)
    }
  }


  "getTextSeq" should "produce a list of text strings, where each string is the text annotated by\n" +
  "the provided annotation type" in {
    assertResult(Seq("abcd", "efgh", "ijkl", "mnop", "qrst", "uvwx", "yz12", "3456", "7")) {
      annotator4.getTextSeq("quail").map(_._2)
    }

    assertResult(Seq("abcdefghijkl", "mnopqrstuvwx")) {
      annotator4.getTextSeq("falcon").map(_._2)
    }

    assertResult(Seq("abcd", "mnop", "yz12")) {
      annotator4.getTextSeq("penguin").map(_._2)
    }
  }

  "getFilteredTextSeq" should "raise an exception if the first annotation type\n" +
  "does not descend from the second annotation type" in {
    val _ = intercept[IllegalArgumentException] {
      annotator4.getFilteredTextSeq("penguin", "falcon").map(_._2)
    }
  }

  it should "produce a list of text strings, where each string is annotated by the second annotation type\n" +
  "and is also partly annotated by the first annotation type" in {

    assertResult(Seq("abcd", "ijkl", "mnop", "uvwx")) {
      annotator4.getFilteredTextSeq("falcon", "quail").map(_._2)
    }

    assertResult(Seq("abcd", "mnop", "yz12")) {
      annotator4.getFilteredTextSeq("penguin", "quail").map(_._2)
    }

  }

}




/*



2.2. XAFS Data Collection
    Titanium pre-edge and XANES spectra for model compounds
were collected in the transmission mode at the LURE facility (Labor-
atoim pour lâutilisation du Rayonnement Electromagnetique, Grsay,
France). The DC1 storage ring was operating at 1.8 GeV and lO@-
300 positron current. Dam were collected at the Ti K-edge (4966
eV) on beam station EXAFS 4. Because only high-resolution pre-
edge data can be used to derive accurate information from these
small features, Si- (3 11) or Si- ( 220) double-crystal monochromators
(detuned 60% to eliminate most of the higher energy harmonics)
and a 0.3 mm vertical slit (before the monochromator) were used
during data collection (0.05 or 0.1 eV steps, energy resolution= 1.2
eV at 5 keV) . Si-( 111) monochromators were also used to measure
low-resolution pre-edge features for comparison with high-resolution
data. The energy calibration of the monochromator was checked
between each spectrum using a Ti metal foil to provide energy
reproducibility at or below 0.05 eV in the experimental spectra.
XAFS data for all Ti model compounds were collected under a
vacuum of 10e3 atm. to minimize hydration. Helium gas was used
in the incident-beam ionization chamber, whereas the transmission
ionization chamber was filled with Ar gas. Samples were prepared
by deposition of powders of these materials on Kapton tape (thick-
nesses of 20-50 pm for an absorbance of p ~2 at 5 keV; edge
jump between 0.5 and 1.5).
2.3. XAFS Data Analysis
2.3.1. TUNES spectra
    The Ti pre K-edge feature shows variations in position and height
for various Ti model compounds, as was also observed by Waychu-
nas ( 1987) and Behrens et al. ( 1990). Both of these pre-edge param-
 eters should be determined independently from the other XAFS fea-
tures (resonances, oscillations, etc) ; otherwise, the information ob-
tained about Ti coordination number may be misleading (cf.;
 Dingwell et al., 1994; Paris et al., 1994). In order to derive informa-
 tion on their height, the pre-edges were normalized in absorbance
 by fitting the spectral region from 4850-4950 eV (the region before
 the pre-edge) using a Victoreen function and subtracting this as
 background absorption. Then, the pre-edges were normalized for
 atomic absorption, based on the average absorption coefficient of
 the spectral region from 5050-5200 eV (after the edge crest) (Fig.
 1). Pre-edge position, height, half-width, and area (Table 1) were
 extracted from the normalized pre-edge spectra by fitting Lorentzians




Table 1: Pm-edge data for selected Ti-containing model compounds.
                    Ti     monochromator                    ls*3d
                 coordin-     information                plPedge data
                 ination      type     step    position normalized PWHhl peak
                                       (eV)     (eV) â    height     (eV) area
Ni2.6Tio.P4         LIlTi Si-(311)      0.05    4969.7     0.94       0.8  1.2
Niz.Oio.&t.o504    t4lTi S&(220)        0.10    4969.6     0.90       1.1  1.0
p-BazTi04          [âITi Si-(311)       0.10    4969.4     1.00       0.8  1.2
aâ-Ba2Ti04         I4]Ti Si-(311)       0.10    4969.5     1.00       0.8  1.2


bio="
  | |P           | {type: {penguin: p}, unit: quail}
  | |f       $   | {type: {falcon: f}, unit: quail}
  | |q~~$q~~$q~~$| {type: {quail: q}, unit: char}
  | |          1 |
  | |012345678901|
   ">abcdefghijkl</tspan>
bio="
  | |P           P  | {type: {penguin: p}, unit: quail}
  | |f       $      | {type: {falcon: f}, unit: quail}
  | |q~~$q~~$q~~$q~~| {type: {quail: q}, unit: char}
  | |1       2      |
  | |234567890123456|
   ">mnopqrstuvwxyz1</tspan>
bio="
  | |$q~~$Q| {type: {quail: q}, unit: char}
  | |2  3  |
  | |789012|
   ">234567</tspan>
  <annotation-links>
    <employment employer="penguin 24" employee="quail 32" />
    <employment employer="penguin 24" employee="quail 28" />
    <employment employer="penguin 12" employee="quail 4" />
  </annotation-links>

 */
