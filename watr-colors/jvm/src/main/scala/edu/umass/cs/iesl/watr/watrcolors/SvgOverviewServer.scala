package edu.umass.cs.iesl
package watr
package watrcolors


object SvgOverviewServer extends SvgOverviewApi  {

  def createView(): List[HtmlUpdate] = {
    List(
      HtmlReplace("#explorer", html.CorpusExplorerPane.init().render)
    )
  }

}
