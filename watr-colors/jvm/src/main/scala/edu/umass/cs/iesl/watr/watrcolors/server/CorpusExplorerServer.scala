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
    println(s"getFileInFocus: ${corpusPath}")
    corpusPath.toString()
  }

  def navPrev(): List[HtmlUpdate] = {
    state = state.prev

    List(
      HtmlReplaceInner(s"#currfile", s"${state.curr.name}")
    )
  }


  def openFocus(): List[HtmlUpdate] = {
    // switch to svg-view
    List(
      // HtmlPrepend(s"#winstack-top", s"${corpusCursor.curr}")
    )
  }


  def createView(): List[HtmlUpdate] = {
    List(
      HtmlReplaceInner("#main", html.CorpusExplorerView.init().render)
    )
  }

}
