package edu.umass.cs.iesl.watr
package watrcolors

import better.files._


case class DirectoryCursor(
  curr:File,
  prevs: Seq[File]  = Seq(),
  nexts: Seq[File] = Seq()
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

  def init(f: File): Option[DirectoryCursor] = {

    if (f.isDirectory) {
      val m = f.glob("**/*.d").toList

      m.headOption.flatMap{ file1 =>
        Option(DirectoryCursor(curr = file1, nexts = m.drop(1).toSeq))
      }

    } else {
      None
    }
  }


}
