package edu.umass.cs.iesl.watr
package watrcolors

import ammonite.ops._

case class DirectoryCursor(
  curr:Path,
  prevs: Seq[Path]  = Seq(),
  nexts: Seq[Path] = Seq()
) {

  def prev: DirectoryCursor= {
    if (prevs.isEmpty) this
    else DirectoryCursor(
      curr = prevs.head,
      prevs = prevs.drop(1),
      nexts = curr +: nexts
    )
  }
  def next: DirectoryCursor = {
    if (nexts.isEmpty) this
    else DirectoryCursor(
      curr = nexts.head,
      prevs = curr +: prevs,
      nexts = nexts.drop(1)
    )
  }
}


object DirectoryCursor {

  def init(f: Path): Option[DirectoryCursor] = {
    val isDir = exists.!(f) && stat.!(f).isDir

    if (isDir) {
      val fs = ls.!(f)
      fs.headOption.flatMap{ file1 =>
        Option(DirectoryCursor(curr = file1, nexts = fs.drop(1).toSeq))
      }
    } else None
  }



}
