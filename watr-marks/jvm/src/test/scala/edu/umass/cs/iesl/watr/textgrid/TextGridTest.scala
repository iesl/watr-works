package edu.umass.cs.iesl.watr
package textgrid

import TypeTags._
import textboxing.{TextBoxing => TB}, TB._


class TextGridTests extends TextGridSpec {

  val stableId = DocumentID("SampleDocument")

  info("behavious of textgrid char-level labeling")


  info("behavious of textgrid row-level labeling")

  "it should apply BIO labels to rows of text" in {
    val rowCount = 10
    val initialText = "abc def\n" * rowCount

    val textGrid = stringToPageTextGrid(stableId, initialText,  PageNum(1), None)


  }

  info("behavious of textgrid block-level labeling")



  // info("behavior of TextGrid Rows")


  // "it should join two rows into one, with optional space or dehyphenation" in {}

  // it should "support alphabet soup regions" in {}
  // it should "clip single row to target region(s)" in {}
  // it should "clip multiple rows to target region(s)" in {}

  // behavior of "TextGrid Cursors"

  // it should "slurp/barf" in {}


  // "it should find the span of cells that have a particular label" - {
  //   val unlabeledText = "11\n22\n33"
  //   val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
  //   textGrid.rowAt(1).foreach{ _.addBioLabel(Author) }

  //   val offset = textGrid.indexAt(1, 1).get

  //   textGrid.findLabelExtents(offset, Author).foreach { case (begin, authorLabeled) =>
  //     begin shouldEqual 2
  //     authorLabeled.length shouldEqual 2
  //   }

  //   textGrid.split(1, 1).foreach { splitGrid =>
  //     val off1 = textGrid.indexAt(1, 0).get
  //     splitGrid.findLabelExtents(off1, Author).foreach { case (begin, extent)=>
  //       begin shouldEqual 2
  //       extent.length shouldEqual 2
  //     }
  //     val off2 = textGrid.indexAt(2, 0).get
  //     splitGrid.findLabelExtents(off2, Author).foreach {case (begin, extent) =>
  //       begin shouldEqual 4
  //       extent.length shouldEqual 2
  //     }
  //   }
  // }


  // "it should find the span of cells that have identical labeling (including perhaps unlabeled)" in {
  //   val unlabeledText = "11\n22\n33"
  //   val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
  //   textGrid.rowAt(1).foreach{ _.addBioLabel(Author) }

  //   def indexAt(r: Int, c: Int) :Int = {
  //     textGrid.indexAt(r, c).get
  //   }
  //   textGrid.findIdenticallyLabeledSiblings(indexAt(0, 1)).foreach { extent =>
  //     val res = extent.map(c => (c._2, c._3))
  //     res shouldEqual Seq((0, 0), (0, 1))
  //   }
  //   textGrid.findIdenticallyLabeledSiblings(indexAt(1, 0)).foreach { extent =>
  //     val res = extent.map(c => (c._2, c._3))
  //     res shouldEqual Seq((1, 0), (1, 1))
  //   }

  //   textGrid.split(1, 1).foreach { splitGrid =>

  //     splitGrid.findIdenticallyLabeledSiblings(indexAt(1, 0)).foreach { extent =>
  //       val res = extent.map(c => (c._2, c._3))
  //       res shouldEqual Seq((1, 0), (2, 0))
  //     }
  //     splitGrid.findIdenticallyLabeledSiblings(indexAt(2, 0)).foreach { extent =>
  //       val res = extent.map(c => (c._2, c._3))
  //       res shouldEqual Seq((1, 0), (2, 0))
  //     }

  //     textGrid.rowAt(2).foreach{ _.addBioLabel(FirstName) }

  //     splitGrid.findIdenticallyLabeledSiblings(indexAt(1, 0)).foreach { extent =>
  //       val res = extent.map(c => (c._2, c._3))
  //       res shouldEqual Seq((1, 0))
  //     }
  //     splitGrid.findIdenticallyLabeledSiblings(indexAt(2, 0)).foreach { extent =>
  //       val res = extent.map(c => (c._2, c._3))
  //       res shouldEqual Seq((2, 0))
  //     }
  //   }
  // }

}
