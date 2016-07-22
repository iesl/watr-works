package edu.umass.cs.iesl.watr
package watrcolors

import GeometricFigure._

trait CorpusExplorerApi {
  def navNext()     : List[HtmlUpdate]
  def navPrev()     : List[HtmlUpdate]
  def openFocus()   : List[HtmlUpdate]
  def getCorpusEntryInFocus() : String
  def createView()    : List[HtmlUpdate]
}


trait SvgOverviewApi {
  def createView(artifactId: String): List[HtmlUpdate]
  def getLabelOverlay(artifactId: String): List[TraceLog]
  def onSelectLTBounds(artifactId: String, bbox: LTBounds): List[HtmlUpdate]
  def onDrawPath(artifactId: String, path: Seq[Point]): List[HtmlUpdate]
}

trait VisualTraceApi {
  def createView(): List[HtmlUpdate]

  def runTrace(): List[TraceLog]

}
