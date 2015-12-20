package edu.umass.cs.iesl.watr
package watrmarks

case class BrickCursor(
  label: BioLabel,
  current: List[LabeledColumn],
  prevs: List[LabeledColumn] = List(),
  nexts: List[LabeledColumn] = List()
) {

  def fold(f: (BrickCursor) => Unit): Unit  = {

    f(this)
    next match {
      case Some(n) => n.fold(f)
      case None => ()
    }
  }

  def next: Option[BrickCursor] = {
    nexts.headOption.map{n =>
      BrickCursor(
        label,
        List(n),
        current ++ prevs,
        nexts.drop(1)
      )
    }
  }

  def addLabel(label: BioLabel): BrickCursor = {
    this
  }

  def toLabeledSpan = LabeledSpan(
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
    s"""< ${prevs.reverse.map(_.char).mkString(" <- ")} ..[${label}:${current.mkString(" :: ")}].. ${nexts.map(_.char).mkString(" -> ")}>""".stripMargin

  }

}
