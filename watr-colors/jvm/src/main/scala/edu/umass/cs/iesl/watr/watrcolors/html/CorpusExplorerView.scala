package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.Text.all._



object CorpusExplorerView {

  def init()  = {
    <.div(
      <.ul("corpus-entries".id
      ),
      <.div()("Explore")(
        <.span(^.id:="currfile")
      )
    )
  }


}

