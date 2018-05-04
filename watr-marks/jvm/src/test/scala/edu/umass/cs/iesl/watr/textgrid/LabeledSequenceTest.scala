package edu.umass.cs.iesl.watr
package textgrid

import geometry._

import TypeTags._
import LabeledSequenceTreeTransforms._

import _root_.io.circe
import circe.syntax._

// import utils.DoOrDieHandlers._
import LabelTreeCodecs._
import TextGridLabelWidget._
  // import textboxing.{TextBoxing => TB}, TB._
  // import utils.{Cursor, Cursors, Window}


class LabeledSequenceTest extends TextGridSpec {

  "Format of Labeled Sequence serialization" in {
    //  inline bio <--> label tree <--> json rep
    val things = unlabeledThings(10)

    for {
      gridCursor <- things.toCursor()
      c3         <- gridCursor.move(3)
      c4 = {
      }

       window = c3.toWindow()
       w2 <- window.widen(3)
      // w2.addLabel(FirstName)
      // w2.closeWindow()
      // c5 <-c4.next
      // _ = {
      //   val window = c5.toWindow()
      //   val w2 = window.widen(3)
      //   w2.addLabel(LastName)
      //   w2.closeWindow()
      // }
    } yield ()

    // val labelTree = gridCellsToLabelTree(inlineBio.cells)

    println(things.labelTargets.map(_.showPinsVert()).mkString)

    // println(labelTree.drawTree)


    // {
    //   println("Marginal Span Tree -----------------------")
    //   val labelSpanTree2 = labelTreeToMarginalSpanTree(labelTree)
    //   println(labelSpanTree2.drawTree)
    //   println("-----------------------")
    // }

    // val labelSpanTree = labelTreeToSpanTree(labelTree)

    // {
    //   println("Label Span Tree -----------------------")
    //   println(labelSpanTree.drawTree)
    //   println("-----------------------")
    // }
    // val jsonSpanTreeRep = spanTreeToJson(labelSpanTree)
    // println(jsonSpanTreeRep.pretty(JsonPrettyPrinter))

    // Json -> inline BIO

    val ls = LabelSpan(LastName, 2, 4)

    val lt = LabelingTree(
      LabelSpan(FirstName, 0, 10), List(
        LabelingTree(
          LabelSpan(LastName, 2, 4), List()
        )
      )
    )

    val lsjs = ls.asJson
    println(lsjs.noSpaces)
    val ltjs = lt.asJson
    println(ltjs.noSpaces)

    // val labelingTree = jsonSpanTreeRep.decodeOrDie[Seq[LabelingTree]]()
    // println(labelingTree)
    // val inlineBioRT = LabelTreeCodecs.decodeBioLabels(jsonSpanTreeRep)
    println("inline Bio Round Trip")
    // val rt = inlineBioRT.map(_.mkString).zipWithIndex.mkString("\n  ", "\n  ", "\n")
    // println(rt)

  }
}
