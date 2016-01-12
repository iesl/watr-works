package edu.umass.cs.iesl
package watr
package watrcolors


object SvgOverviewServer extends SvgOverviewApi  {

  def createView(): List[HtmlUpdate] = {
    List(
      HtmlReplace("#main", html.SvgOverviewPane.init().render)
    )
  }

}
