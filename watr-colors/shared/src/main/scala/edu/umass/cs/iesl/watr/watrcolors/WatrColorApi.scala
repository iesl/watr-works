package edu.umass.cs.iesl.watr
package watrcolors



trait CorpusExplorerApi {
  def navNext()     : List[HtmlUpdate]
  def navPrev()     : List[HtmlUpdate]
  def openFocus() : List[HtmlUpdate]
  def createView()    : List[HtmlUpdate]

}

trait SvgOverviewApi {
  def createView()    : List[HtmlUpdate]
}




