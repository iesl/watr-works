package edu.umass.cs.iesl.watr
package watrcolors
package client

import scaladget.stylesheet.{all => sheet}
import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
// import scalatags.JsDom.{ styles, svgAttrs }
import scalatags.JsDom.{ styles  }
import sheet._
import bs._
import utils.Colors


object PageLayout {
  lazy val footerStyle: ModifierSeq = Seq(
    padding:="0",
    margin:="0",
    border := "0",
    backgroundColor := "black",
    styles.color := Colors.Red.toCSSStr,
    left:="0"

  )

  lazy val canvasContainer: ModifierSeq = Seq(
    padding:="0",
    border:="0",
    margin:="0",
    position.relative
  )

  lazy val fabricCanvas: ModifierSeq = Seq(
    position.absolute,
    padding:="0",
    margin:="0",
    border := "0",
    left:="0",
    zIndex:=100,
    top:="0"
  )

  def initLabelerPage() = {
    // Create nav items
    val oneItem = stringNavItem("One", () ⇒
      println("One open")
    )

    val twoItem = stringNavItem("Two", () ⇒
      println("Two open"), true
    )

    val threeItem = navItem(
      bs.input("")(placeholder := "Name", width := 100).render, () ⇒
      println("Three open")
    )

    val fourItem = navItem(
      div(glyph_fire +++ (color := "#337ab7"), lineHeight := "35px").render, () ⇒
      println("Four open")
    )

    val fiveItem = navItem(
      buttonGroup()(
        bs.button("OK", btn_primary, ()=> {println("Five open")}),
        bs.button("Cancel", btn_default, ()=> {println("Five cancel")})
      ).render
    )

    //Create the nav bar
    val navbar = bs.navBar(
      navbar_staticTop,
      oneItem,
      twoItem,
      threeItem,
      fourItem,
      fiveItem
    )
    // def statusbar()  = {
    //   <.div(^.id:="status-bar", WatrStyles.statusBar)(
    //     <.div(^.id:="status-controls", WatrStyles.statusCtrls),
    //     <.div(^.id:="status-text", WatrStyles.statusText)
    //   )
    // }




    div(
      navbar,
      div(
        div(sheet.marginLeft(15), sheet.marginTop(25))(
          div(^.id:="canvas-container", canvasContainer)(
            canvas(^.id:="canvas", fabricCanvas)
          )
        )
      ),
      footer(footerStyle)(
        div(row)(
          div(colMD(12))()
        )
      )
    )


  }
}
