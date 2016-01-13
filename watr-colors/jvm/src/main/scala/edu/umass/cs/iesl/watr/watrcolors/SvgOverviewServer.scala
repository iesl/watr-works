package edu.umass.cs.iesl
package watr
package watrcolors


object SvgOverviewServer extends SvgOverviewApi  {

  def createView(svgFilename: String): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", html.SvgOverviewPane.init(svgFilename).toString)
    )
  }

}
