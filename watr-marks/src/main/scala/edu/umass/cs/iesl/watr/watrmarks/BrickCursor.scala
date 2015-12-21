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


  def addLabel(label: BioLabel): BrickCursor = {
    this
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

}
