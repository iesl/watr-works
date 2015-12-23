package edu.umass.cs.iesl.watr
package watrmarks

import textboxing.TextBoxing

import dom._


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
    val (_, lastBrickCursor) = focii.last

    val combined = BioCursor.combineCursors(head, tail)

    lastBrickCursor.next.map({nextBrickCursor =>
      BioCursor.initCursorFwd(
        lastBrickCursor.label,
        combined,
        Some(nextBrickCursor))
    }).getOrElse {
      combined.nextTSpan.flatMap({nextTspan =>
        BioCursor.initCursorFwd(
          lastBrickCursor.label,
          nextTspan,
          None
        )
      })
    }

  }


  def foreach(f: (BioCursor) => Unit): Unit = {
    println("running foreach ")

    f(this)
    next match {
      case Some(ncur) => ncur.foreach(f)
      case None =>
    }
  }

  // override def toString = {
  //   "biocursor<>"
  // }

  def showBox: TB.Box = {
    focii.map{ case (dcur, bcur) =>
      dcur.showBox atop bcur.showBox
    }.mkVBox() |> TB.border
  // }.mkVBox() |> TB.borderLeftRight("<", "")
  }

}

object BioCursor {

  def combineCursors(
    head: (DomCursor, BrickCursor),
    tail: Seq[(DomCursor, BrickCursor)]
  ): DomCursor = {

    val (hdcur, hbcur) = head
    val focii = head+:tail

    val updatedHead = hdcur.modifyLabel { t =>
      t.asInstanceOf[TSpan].copy(
        bioBrick = hbcur.toBrickColumns
      )
    }

    // debugReport("updated head",
    //   updatedHead.getLabelAsTSpan.bioBrick.showBox.padTop1
    // )

    val foldedFocii = tail
      .foldLeft(updatedHead)({ (accDCur,dbcurs) =>
        val (dcur, bcur) = dbcurs

        val nextTSpanOpt = accDCur.nextTSpan
        assert(nextTSpanOpt.isDefined, "error advancing BioCursor")

        val nextTSpan = nextTSpanOpt.get

        assert(nextTSpan.getLabel == dcur.getLabel, "error advancing BioCursor")

        val modifiedTspan = nextTSpan.modifyLabel{ t =>
          t.asInstanceOf[TSpan].copy(
            bioBrick = bcur.toBrickColumns
          )
        }

        // debugReport("next tspan",
        //   nextTSpan.getLabelAsTSpan.bioBrick.showBox.padTop1
        // )
        // debugReport("incoming acc",
        //   accDCur.getLabelAsTSpan.bioBrick.showBox.padTop1
        // )
        // debugReport("modified acc",
        //   modifiedTspan.getLabelAsTSpan.bioBrick.showBox.padTop1
        // )

        modifiedTspan
      })

    val (ldcur, lbcur) = focii.last

    foldedFocii
  }

  // def searchForwardForLabel(label: BioLabel, odcur: Option[DomCursor], searchForBegin:Boolean): Seq[DomCursor] = {
  def searchForwardForLabel(label: BioLabel, odcur: Option[DomCursor], searchFor:BeginEnd): Seq[DomCursor] = {
    import scalaz.std.stream._

    val elems = unfold[Option[DomCursor], DomCursor](
      odcur
    )(_ match {
      case Some(dcur) =>
        val thisDomCoversLabel =
          dcur.getLabelAsTSpan
            .bioBrick
            .initBrickCursor(label)
            .exists({c =>
              (searchFor==Begin && c.coversStartOfLabel) ||
                (searchFor==End && c.coversEndOfLabel)
            })

        if (thisDomCoversLabel)
          Some((dcur, None))
        else
          Some((dcur, dcur.nextTSpan))



        // val maybeNextTSpanCur = dcur.nextTSpan

        // val maybeFoundEnd = maybeNextTSpanCur
        //   .map({ _.getLabelAsTSpan
        //     .bioBrick
        //     .initBrickCursor(label)
        //     .exists({c =>
        //       (searchFor==Begin && c.coversStartOfLabel) ||
        //       (searchFor==End && c.coversEndOfLabel)
        //     })
        //   })

        // maybeFoundEnd.fold(
        //   ifEmpty =Some((maybeNextTSpanCur.get, maybeNextTSpanCur))
        // )(f =>
        //   Some((maybeNextTSpanCur.get, None))
        // )

      case None => None
    })

    elems
  }

  // possibilities:
  //    startingDomCursor should *always* be focused on a TSpan
  //      startingBrickCursor can be Some or None

  def initCursorFwd(label: BioLabel, dcur: DomCursor, bcur: Option[BrickCursor] = None): Option[BioCursor] = {
    assert(dcur.getLabel.isInstanceOf[TSpan])

    debugReport("init forward cursor",
      dcur.showBox.padTop1,
      dcur.getLabel,
      bcur.map(_.showBox.padTop1)
    )

    bcur match {
      case Some(brickCursor) =>

        // val domStoredBrick = dcur.getLabelAsTSpan.bioBrick
        // val providedBrick = brickCursor.toBrickColumns

        // debugReport(domStoredBrick.showBox.padTop1)
        // debugReport(providedBrick.showBox.padTop1)

        // debugReport(brickCursor.showBox.padTop1)
        // assert(dcur.getLabelAsTSpan.bioBrick == brickCursor.toBrickColumns)
        // debugReport(brickCursor.showBox.padTop1)

        if (brickCursor.coversEndOfLabel) {
          // debugReport("brickCursor.coversEndOfLabel")
          Some(BioCursor((dcur, brickCursor),Seq()))
        } else {
          // debugReport("!brickCursor.coversEndOfLabel")
          // val labelCoverBox = labelCover.map(_.showBox).mkVBox().padTop1
          // debugReport(labelCoverBox)
          val labeledDCursors = searchForwardForLabel(label, Some(dcur), searchFor=End)

          labeledDCursors.headOption.map({ hdcur =>
            BioCursor(
              (hdcur, brickCursor),
              labeledDCursors.tail.map({dc =>
                (dc, domBrickCursor(dc, label).get)
              })
            )
          })
        }


      case None => // No starting brick cursor
        for {
          beginDCursor <- searchForwardForLabel(
            label, Some(dcur), searchFor=Begin
          ).lastOption

          fullCursor <- initCursorFwd(
            label, beginDCursor, domBrickCursor(beginDCursor, label)
          )
        } yield fullCursor
    }

  }
  def domBrick(d: DomCursor) =
    d.getLabelAsTSpan.bioBrick

  def domBrickCursor(d: DomCursor, l: BioLabel) =
    domBrick(d).initBrickCursor(l)
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



  def initCursorFwd(label: BioLabel, dcur: DomCursor, bcur: Option[BrickCursor] = None): Option[BioCursor] = {
    assert(dcur.getLabel.isInstanceOf[TSpan])

    debugReport("init forward cursor", dcur.showBox.padTop1)
    debugReport("init forward cursor", dcur.getLabel)


    val nextBioCursor = bcur match {
      case Some(brickCursor) =>

        val domStoredBrick = dcur.getLabelAsTSpan.bioBrick
        val providedBrick = brickCursor.toBrickColumns

        debugReport(domStoredBrick.showBox.padTop1)
        debugReport(providedBrick.showBox.padTop1)

        debugReport(brickCursor.showBox.padTop1)
        assert(dcur.getLabelAsTSpan.bioBrick == brickCursor.toBrickColumns)

        brickCursor.next match {
          case Some(nextBrickCursor) =>
            debugReport(nextBrickCursor.showBox.padTop1)
            if (nextBrickCursor.coversEndOfLabel) {
              debugReport("nextBrickCursor.coversEndOfLabel")
              Some(BioCursor((dcur, nextBrickCursor),Seq()))
            } else {
              debugReport("!nextBrickCursor.coversEndOfLabel")
              val labelCover = searchForwardForLabel(label, Some(dcur), searchForBegin=false)
              // val labelCoverBox = labelCover.map(_.showBox).mkVBox().padTop1
              // debugReport(labelCoverBox)

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


      case None => // No starting brick cursor
        searchForwardForLabel(label, Some(dcur), searchForBegin=true)
          .lastOption.map({ beginDCursor =>
            initCursorFwd(label, beginDCursor,
              domBrickCursor(beginDCursor, label)
            )
          })
    }
    nextBioCursor
  }

 */
