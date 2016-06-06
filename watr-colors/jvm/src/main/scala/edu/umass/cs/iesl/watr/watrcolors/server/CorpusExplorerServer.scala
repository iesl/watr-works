package edu.umass.cs.iesl.watr
package watrcolors
package server


import ammonite.ops._

class CorpusExplorerServer(
  rootDirectory: Path
) extends CorpusExplorerApi  {

  // val initpath = rootDirectory
  println(s"CorpusExplorerServer current dir=${cwd}, corpus root=${rootDirectory}")

  val init = DirectoryCursor.init(rootDirectory).get

  var state: DirectoryCursor = init

  def navNext(): List[HtmlUpdate] = {
    state = state.next
    createView()
  }

  def getCorpusEntryInFocus() : String = {
    // val corpusPath = rootDirectory.relativize(state.curr.path)
    // val corpusPath = rootDirectory.relativeTo(state.curr)
    val entryPath = state.curr.relativeTo(rootDirectory)
    println(s"CorpusExplorerServer:getCorpusEntryInFocus(${state.curr})= '${entryPath}' // corpus root=${rootDirectory}")
    entryPath.toString
  }

  def navPrev(): List[HtmlUpdate] = {
    state = state.prev
    createView()
  }

  def openFocus(): List[HtmlUpdate] = {
    List()
  }

  def createView(): List[HtmlUpdate] = {
    val initHtml = html.CorpusExplorerView.init(state)
    List(
      HtmlReplaceInner("#main", initHtml.render)
    )
  }

}
