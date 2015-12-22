package edu.umass.cs.iesl.watr.watrmarks

import dom._

import com.softwaremill.debug.DebugConsole._

case class BioCursor(
  head: (DomCursor, BrickCursor),
  tail: Seq[(DomCursor, BrickCursor)]
) {

  lazy val focii = head+:tail

  def getText: String =  {
    focii.map({ case (dcur, bcur) =>
      bcur.current.map(_.char).mkString
    }).mkString
  }

  def next: Option[BioCursor] = {

    println("next()")

    val (hdcur, hbcur) = head

    println(s"hdcur = ${hdcur}")
    println(s"hbcur = ${hbcur}")

    val updatedHead = hdcur.modifyLabel { t =>
      t.asInstanceOf[TSpan].copy(
        bioBrick = hbcur.toBrickColumns
      )
    }

    val foldedFocii = tail
      .foldLeft(updatedHead)({ (accDCur,dbcurs) =>
        val (dcur, bcur) = dbcurs

        val nextTSpanOpt = accDCur.nextTSpan
        assert(nextTSpanOpt.isDefined, "error advancing BioCursor")

        val nextTSpan = nextTSpanOpt.get

        assert(nextTSpan.getLabel == dcur.getLabel, "error advancing BioCursor")

        nextTSpan.modifyLabel{ t =>
          t.asInstanceOf[TSpan].copy(
            bioBrick = hbcur.toBrickColumns
          )
        }
      })

    val (ldcur, lbcur) = focii.last

    BioCursor.initCursorFwd(hbcur.label, foldedFocii, Some(lbcur))
  }


  def foreach(f: (BioCursor) => Unit): Unit = {
    println("running foreach ")

    f(this)
    next match {
      case Some(ncur) => ncur.foreach(f)
      case None =>
    }
  }

  override def toString = {
    val fs = focii.map{ case (dcur,bcur) =>

      // println(s"dcur: ${dcur}")
      // println(s"bcur: ${bcur}")

      s""" ${dcur.toString}
      ${bcur.toString}"""
    }.mkString("\n")

    s"cur<${fs}>"
  }

}

object BioCursor {

  def searchForwardForLabelEnd(label: BioLabel, odcur: Option[DomCursor]): Seq[DomCursor] = {
    import scalaz.std.stream._

    val elems = unfold[Option[DomCursor], DomCursor](
      odcur
    )(_ match {
      case Some(dcur) =>
        val maybeNextTSpanCur = dcur.nextTSpan

        val maybeFoundEnd = maybeNextTSpanCur
          .map({ _.getLabelAsTSpan
            .bioBrick
            .initBrickCursor(label)
            .exists(_.coversEndOfLabel)
          })

        maybeFoundEnd.fold(
          ifEmpty =Some((maybeNextTSpanCur.get, maybeNextTSpanCur))
        )(f =>
          Some((maybeNextTSpanCur.get, None))
        )

      case None => None
    })

    elems
  }

  def initCursorFwd(label: BioLabel, dcur: DomCursor, bcur: Option[BrickCursor] = None): Option[BioCursor] = {
    println("initCursorFwd(): ")
    println(s"init dcur = ${dcur}")
    println(s"init bcur = ${bcur}")

    val nextBioCursor = bcur match {
      case Some(brickCursor) =>
        assert(dcur.getLabelAsTSpan.bioBrick == brickCursor.toBrickColumns)
        debugReport(brickCursor)

        brickCursor.next match {
          case Some(nextBrickCursor) =>
            debugReport(nextBrickCursor)
            if (nextBrickCursor.coversEndOfLabel) {
              debugReport("nextBrickCursor.coversEndOfLabel")
              Some(BioCursor((dcur, nextBrickCursor),Seq()))
            } else {
              debugReport("!nextBrickCursor.coversEndOfLabel")
              val labelCover = searchForwardForLabelEnd(label, dcur.nextTSpan)

              Some(BioCursor(
                (dcur, nextBrickCursor),
                labelCover.map({dc =>
                  (dc, dc.getLabelAsTSpan.bioBrick.initBrickCursor(label).get)
                })
              ))
            }

          case None =>
            val labelCover = searchForwardForLabelEnd(label, dcur.nextTSpan)

            Some(
            BioCursor(
              (labelCover.head, labelCover.head.getLabelAsTSpan.bioBrick.initBrickCursor(label).get),
              labelCover.tail
                .map({dc =>
                  (dc, dc.getLabelAsTSpan.bioBrick.initBrickCursor(label).get)
                })
            ))
        }


      case None =>
        // val startingTSpan = if (dcur.getLabel.isInstanceOf[TSpan]) {
        //   Some(dcur)
        // } else {
        //   dcur.nextTSpan
        // }

        // startingTSpan



        // val labelCover = searchForwardForLabelEnd(label, startingTSpan)
        val labelCover = searchForwardForLabelEnd(label, Some(dcur))
        println("labelCover: "+labelCover.mkString(","))
        labelCover.headOption.map{ lchead =>
          BioCursor(
            (lchead, lchead.getLabelAsTSpan.bioBrick.initBrickCursor(label).get),
            labelCover.tail
              .map({dc =>
                (dc, dc.getLabelAsTSpan.bioBrick.initBrickCursor(label).get)
              })
          )

        }

    }
    nextBioCursor
  }
}









    // val treeLoc = dcur.loc

    // val nodesStartingWithLabel = treeLoc
    //   .cojoin
    //   .toTree.flatten
    //   .filter(_.getLabel.isInstanceOf[TSpan])
    //   .dropWhile({ domloc =>
    //     val tspan = domloc.getLabel.asInstanceOf[TSpan]
    //     tspan.bioBrick.initBrickCursor(label).isEmpty
    //   })


    // val nodesWithLabelMinusOne =
    //   nodesStartingWithLabel
    //     .takeWhile({ domloc =>
    //       val tspan = domloc.getLabel.asInstanceOf[TSpan]
    //       val bioBrick = tspan.bioBrick
    //       bioBrick.initBrickCursor(label).exists {
    //         bcur => !bcur.coversEndOfLabel
    //       }
    //     })

    // val nodesWithLabel =  nodesWithLabelMinusOne ++ nodesStartingWithLabel.drop(nodesWithLabelMinusOne.length).take(1)


    // val nodeBrickCursorPairs =
    //   nodesWithLabel.map({nloc => (
    //     DomCursor(nloc),
    //     nloc.getLabel.asInstanceOf[TSpan].bioBrick.initBrickCursor(label).get
    //   )})

    // nodeBrickCursorPairs.headOption.map{ h =>
    //   BioCursor(h, nodeBrickCursorPairs.tail)
    // }
/*


*/
