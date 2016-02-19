package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.Text.all._


object CorpusExplorerPane {

  def init()  = {
    <.div(
      <.div()("Explore")(
        <.span(^.id:="currfile")
      )
    )
  }

}

