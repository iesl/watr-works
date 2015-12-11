package edu.umass.cs.iesl.watr
package watrcolors



trait WatrColorApi {
  def list(path: String): Seq[String]

  // def openCorpus

  // def corpusCursorNext(c: CollectiveCursor): CollectiveCursor = {
  //   c.copy(
  //     c.corpusCursor.map{cc =>
  //       cc.next
  //     }
  //   )

  //   c
  // }

  // def fileOpen(): Boolean


  def navNext(): Seq[(String, String)]

  def navNextXX(): String

  // def navPrev(): Boolean

  // def navEnter(): Boolean

}
