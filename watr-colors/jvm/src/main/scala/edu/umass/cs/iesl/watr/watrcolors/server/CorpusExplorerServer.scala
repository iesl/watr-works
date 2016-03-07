package edu.umass.cs.iesl.watr
package watrcolors
package server

import better.files._

class CorpusExplorerServer(
  config: PdfCorpusConfig
) extends CorpusExplorerApi  {

  println(s"CorpusExplorerServer current dir = ${Cmds.cwd}")

  val initpath = File(config.rootDirectory)

  val init = DirectoryCursor.init(initpath).get

  var state: DirectoryCursor = init

  def navNext(): List[HtmlUpdate] = {
    state = state.next

    List(
      HtmlReplaceInner(s"#currfile", s"${state.curr.name}")
    )
  }

  def getFileInFocus() : String = {
    val corpusPath = initpath.relativize(state.curr.path)
    corpusPath.toString()
  }

  def navPrev(): List[HtmlUpdate] = {
    state = state.prev
    List(
      HtmlReplaceInner(s"#currfile", s"${state.curr.name}")
    )
  }

  def openFocus(): List[HtmlUpdate] = {
    List()
  }

  def createView(): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", html.CorpusExplorerView.init().render)
    )
  }

}
