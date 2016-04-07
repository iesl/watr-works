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
  def createView(svgFilename: String): List[HtmlUpdate]
  def getCharLevelOverlay(svgFilename: String): List[BBox]
  def getCermineOverlay(svgFilename: String): List[BBox]
  def onSelectBBox(artifactId: String, bbox: BBox): List[HtmlUpdate]
}
