package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.Text.all._


object CorpusExplorerView {

  def renderCorpusEntries(corpus: Corpus): TextTag = {

    // val ps = dc.prevs.reverse.map{ v =>
    //   <.li(v.name)
    // }
    // val ns = dc.nexts.map{ v =>
    //   <.li(v.name)
    // }

    // <.ul(ps,
    //   <.li(
    //     $.borderWidth:=2,
    //     $.borderStyle:="solid"
    //   )(dc.curr.name),
    //   ns
    // )
    ???
  }


  def renderDirectoryCursor(dc: DirectoryCursor): TextTag = {
    val ps = dc.prevs.reverse.map{ v =>
      <.li(v.name)
    }
    val ns = dc.nexts.map{ v =>
      <.li(v.name)
    }

    <.ul(ps,
      <.li(
        $.borderWidth:=2,
        $.borderStyle:="solid"
      )(dc.curr.name),
      ns
    )
  }

  def init(dc: DirectoryCursor)  = {
    <.div("corpus-entries".id,
      renderDirectoryCursor(dc)
    )
  }


}
