package edu.umass.cs.iesl.watr
package watrmarks

case class BrickCursor(
  label: BioLabel,
  current: List[BrickColumn],
  prevs: List[BrickColumn] = List(),
  nexts: List[BrickColumn] = List()
) {

  def fold(f: (BrickCursor) => Unit): Unit  = {

    f(this)
    next match {
      case Some(n) => n.fold(f)
      case None => ()
    }
  }

  def next: Option[BrickCursor] = {
    BrickColumns.initCursor(label, nexts)
      .map{ nextc => nextc.copy(
        prevs = nextc.prevs ++ (current.reverse ++ prevs)
      )
    }
  }
  def prev: Option[BrickCursor] = {
    BrickColumns.initCursor(label, prevs, searchForward=false)
      .map{ prevc => prevc.copy(
        nexts = prevc.nexts ++ current ++ nexts
      )
    }
  }

  // def shiftFocusToLabel(label: BioLabel): Option[BrickCursor] = {
  //   this.copy(
  //     lab
  //   )
  // }

  def addLabel(label: BioLabel): BrickCursor = {
    val newCurrent = if (current.length==1) {
      current.map({ col=>
        col.copy(pins = col.pins + label.U)
      })
    } else if (current.length > 1) {
      val head = current.head.copy(
        pins = current.head.pins + label.B
      )
      val last = current.last.copy(
        pins = current.last.pins + label.L
      )
      val middle = current.drop(1).dropRight(1).map({ col=>
        col.copy(pins = col.pins + label.U)
      })

      (head :: middle) :+ last
    } else {
      sys.error("zero-length focus in brick cursor")
    }

    this.copy(
      current = newCurrent
    )

  }

  def toBrickColumns = BrickColumns(
    prevs.reverse ++ current ++ nexts
  )

  lazy val hpins = current.head.pins
  lazy val lpins = current.last.pins

  def coversCompleteLabel: Boolean =
    coversStartOfLabel && coversEndOfLabel

  def coversStartOfLabel: Boolean =
    hpins.contains(label.U) || hpins.contains(label.B)

  def coversEndOfLabel: Boolean =
    lpins.contains(label.U) || lpins.contains(label.L) || label==CharLabel

  override def toString = {
    s"""< ${prevs.reverse.map(c => s"'${c.char}").mkString(" ")} ..[${label}:${current.mkString(" :: ")}].. ${nexts.map(c => s"'${c.char}").mkString(" ")}>""".stripMargin
  }

  import TB._

  def showBox: TB.Box = {
    borderInlineTop(
      vjoin()(
        label.showBox,
        prevs.reverse.map(_.showBox) |> hjoin(sep=", "),
        "-> " beside (current.map(_.showBox) |> hjoin(sep=", ")) ,
        nexts.map(_.showBox) |> hjoin(sep=", ")
      )
    )
  }

}
