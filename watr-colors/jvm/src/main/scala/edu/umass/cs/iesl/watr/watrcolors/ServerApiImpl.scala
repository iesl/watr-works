package edu.umass.cs.iesl.watr
package watrcolors

import better.files._
import upickle.Js



trait WatrColorApiServer extends WatrColorApi with ServerState {

  def navNext(): Map[String, String] = {
    val last = corpusCursor
    corpusCursor = last.next

    Map(
      "curr" -> s"${corpusCursor.curr}"
    )
  }

  def navPrev(): Map[String, String] = {
    val last = corpusCursor
    corpusCursor = last.prev

    Map(
      "curr" -> s"${corpusCursor.curr}"
    )
  }

  def openCurrent(): Map[String, String] = {
    Map(
      "expand" -> s"${corpusCursor.curr}"
    )
  }

}

trait ServerState {
  // val initpath = file"../sample-pdf-to-svg/rpp-pdf-test-samples/samples-mit"
  val initpath = file"../../corpus~/samples-mit"
  // directory -> cursor over directory, focus on single file
  // loaded SVG -> display as svg, cursor
  var corpusCursor = DirectoryCursor.init(initpath).get

}

case class DirectoryCursor(
  curr:File,
  prevs: Seq[File]  = Seq(),
  nexts: Seq[File] = Seq(),
  horizon: Iterator[File] = Iterator()
) {
  val horizonDistance = 10

  def prev: DirectoryCursor = {
    if (prevs.isEmpty)
      this
    else DirectoryCursor(
      curr = prevs.head,
      prevs = prevs.drop(1),
      nexts = curr +: nexts,
      horizon
    )
  }
  def next: DirectoryCursor = {
    val prefetchN = Math.max(horizonDistance-nexts.length, 0)
    val newNexts = nexts ++ horizon.take(prefetchN).toSeq

    if (newNexts.isEmpty)
      this
    else DirectoryCursor(
      curr = newNexts.head,
      prevs = curr +: prevs,
      nexts = newNexts.drop(1),
      horizon
    )
  }

}

object DirectoryCursor {

  def init(f: File): Option[DirectoryCursor] = {

    if (f.isDirectory) {
      val m = f.glob("**/*.pdf")

      if (m.hasNext) {
        Option(DirectoryCursor(curr = m.next(), horizon = m))
      } else {
        None
      }

    } else {
      Option(new DirectoryCursor(curr = f))
      None
    }
  }


}
