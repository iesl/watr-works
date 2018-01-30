package edu.umass.cs.iesl.watr
package textgrid

import TypeTags._
import textboxing.{TextBoxing => TB}, TB._


class TextGridTests extends TextGridSpec {

  val stableId = DocumentID("SampleDocument")

  // info("behavior of TextGrid Rows")


  // "it should join two rows into one, with optional space or dehyphenation" in {}

  // it should "support alphabet soup regions" in {}
  // it should "clip single row to target region(s)" in {}
  // it should "clip multiple rows to target region(s)" in {}

  // behavior of "TextGrid Cursors"

  // it should "slurp/barf" in {}


  "it should find the span of cells that have a particular label" - {
    val unlabeledText = "11\n22\n33"
    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    textGrid.labelRow(1, Author)

    textGrid.findLabelExtents(1, 1, Author).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((1, 0), (1, 1))
    }

    textGrid.split(1, 1).foreach { splitGrid =>
      splitGrid.findLabelExtents(1, 0, Author).foreach {extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
      splitGrid.findLabelExtents(2, 0, Author).foreach {extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
    }
  }


  "it should find the span of cells that have identical labeling (including perhaps unlabeled)" in {
    val unlabeledText = "11\n22\n33"
    val textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    textGrid.labelRow(1, Author)
    textGrid.findIdenticallyLabeledSiblings(0, 1).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((0, 0), (0, 1))
    }
    textGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
      val res = extent.map(c => (c._2, c._3))
      res shouldEqual Seq((1, 0), (1, 1))
    }

    textGrid.split(1, 1).foreach { splitGrid =>

      splitGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }
      splitGrid.findIdenticallyLabeledSiblings(2, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0), (2, 0))
      }

      splitGrid.labelRow(2, FirstName)

      splitGrid.findIdenticallyLabeledSiblings(1, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((1, 0))
      }
      splitGrid.findIdenticallyLabeledSiblings(2, 0).foreach { extent =>
        val res = extent.map(c => (c._2, c._3))
        res shouldEqual Seq((2, 0))
      }
    }
  }

  "it should find row reorderings" - {

  }





}
