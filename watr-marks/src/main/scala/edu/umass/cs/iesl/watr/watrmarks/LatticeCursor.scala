package edu.umass.cs.iesl.watr
package watrmarks

case class LatticeCursor(
  label: BioLabel,
  current: List[BioColumn],
  prevs: List[BioColumn] = List(),
  nexts: List[BioColumn] = List()
) {


  def toText: String = {
    current.map(_.char).mkString
  }

  def unfoldLabels: Stream[LatticeCursor] = {
    import scalaz.std.stream._
    unfold[Option[LatticeCursor], LatticeCursor](
      Some(this)
    )(_ match {
      case Some(cur) => Some(cur -> cur.next)
      case _ => None
    })
  }


  def focusedFonts(): List[FontInfo] = {
    current.map(_.fonts).flatten
  }

  def focusedBounds(): List[TextBounds] = {
    current.map(_.bounds).flatten
  }

  def fold(f: (LatticeCursor) => Unit): Unit  = {
    f(this)
    next match {
      case Some(n) => n.fold(f)
      case None => ()
    }
  }

  def next: Option[LatticeCursor] = {
    BioLattice.initCursor(label, nexts, Forward)
      .map{ nextc => nextc.copy(
        prevs = nextc.prevs ++ (current.reverse ++ prevs)
      )
    }
  }

  def prev: Option[LatticeCursor] = {
    BioLattice.initCursor(label, prevs, Forward)
      .map{ prevc => prevc.copy(
        nexts = prevc.nexts ++ current ++ nexts
      )
    }
  }

  def initCursor(l: BioLabel): Option[LatticeCursor] = {
    BioLattice.initCursor(l, current++nexts, Forward)
      .map{ newC => newC.copy(
        prevs=newC.prevs++prevs
      )
    }
  }


  def addLabel(label: BioLabel): LatticeCursor = {
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
        col.copy(pins = col.pins + label.I)
      })

      (head :: middle) :+ last
    } else {
      sys.error("zero-length focus in brick cursor")
    }

    this.copy(
      current = newCurrent
    )

  }

  def toBioLattice = BioLattice(
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

  // override def toString = {
  //   s"""< ${prevs.reverse.map(c => s"'${c.char}").mkString(" ")} ..[${label}:${current.mkString(" :: ")}].. ${nexts.map(c => s"'${c.char}").mkString(" ")}>""".stripMargin
  // }

  import TB._

  def showBox: TB.Box = {
    borderInlineTop(
      vjoin()(
        label.showBox,
        prevs.reverse.map(_.showIcon) |> hjoin(sep=", "),
        "-> " beside (current.map(_.showBox) |> hjoin(sep=", ")) ,
        nexts.map(_.showIcon) |> hjoin(sep=", ")
      )
    )
  }

}
