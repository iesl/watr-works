package edu.umass.cs.iesl.watr
package watrcolors

import better.files._
import upickle.Js


trait WatrColorApiServer extends WatrColorApi {
  def list(path: String) = {
    val chunks = path.split("/", -1)
    val prefix = "./" + chunks.dropRight(1).mkString("/")
    val files = Option(new java.io.File(prefix).list()).toSeq.flatten
    files.filter(_.startsWith(chunks.last))
  }

  def navNext(): Seq[(String, String)] = {
    Seq(
      ".a" -> "<x></x>"
    )
  }

  def navNextXX(): String = { "hi!"}

}



case class CollectiveCursor(
  corpusCursor: Option[DirectoryCursor] = DirectoryCursor.init(file"corpus~/samples-mit/")
)


case class DirectoryCursor(
  curr:File,
  prevs: Seq[File]  = Seq(),
  nexts: Seq[File] = Seq(),
  horizon: Iterator[File] = Iterator()

) {
  val horizonDistance = 10

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
      val m = f.glob("*.pdf")
      if (m.hasNext)
        Option(DirectoryCursor(curr = m.next(), horizon = m))
      else
        None

    } else {
      Option(DirectoryCursor(curr = f))
      None
    }
  }

}



trait ServerState {

  // directory -> cursor over directory, focus on single file
  // loaded SVG -> display as svg, cursor
  val corpusCursor = DirectoryCursor.init(file"corpus~/samples-mit/")


  // start in corpus~



}
