package edu.umass.cs.iesl.watr
package watrcolors
package client


@JSExport
object WatrColorClient {

  var currentView: ClientView = null

  def switchViews(v: ClientView): Unit = {
    currentView = v
    currentView.createView()
    currentView.setKeybindings(currentView.initKeys)
  }

  // TODO Can't figure out why this main() is getting called twice, so putting this guard here..
  var started = false

  @JSExport
  def main(): Unit = {
    if (!started) {
      started = true

      println("WatrColors Client started")

    }
  }
}
