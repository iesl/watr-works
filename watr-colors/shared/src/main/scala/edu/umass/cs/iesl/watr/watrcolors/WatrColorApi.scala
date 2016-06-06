package edu.umass.cs.iesl.watr
package watrcolors

trait CorpusExplorerApi {
  def navNext()     : List[HtmlUpdate]
  def navPrev()     : List[HtmlUpdate]
  def openFocus()   : List[HtmlUpdate]
  def getCorpusEntryInFocus() : String
  def createView()    : List[HtmlUpdate]
}


trait SvgOverviewApi {
  def createView(artifactId: String): List[HtmlUpdate]
  def getCharLevelOverlay(artifactId: String, query: BBox): List[BBox]
  def getDocumentOverlay(artifactId: String): List[BBox]
  def onSelectBBox(artifactId: String, bbox: BBox): List[HtmlUpdate]
}
