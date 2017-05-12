package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages


import scaladget.stylesheet.{all => sty}
import scalatags.JsDom.all.{marginTop => jmarginTop, _}

import scalatags.JsDom.{
  svgAttrs,
  styles
}
import utils.Colors
import sty._


object pageStyles {
  lazy val inlineUList = Seq(
    display.inline,
    margin  := 0,
    padding  := 0,
    listStyleType.none,
    overflow.hidden
  )
  lazy val inlineULI = Seq(
    display.inline,
    padding  := "8px"
  )

  lazy val zeroFringe: ModifierSeq = Seq(
    padding     :=0,
    border      :=0,
    margin      :=0
  )

  lazy val logo: ModifierSeq = Seq(
    color            := Colors.Coral.cssHash,
    fontSize         := "28px"
  )

  lazy val absCenterPage: ModifierSeq = Seq(
    position.fixed,
    top := "45%",
    styles.left := "50%",
    minWidth := 250,
    svgAttrs.transform := "translate (-50%,-50%)"
  )

  lazy val absoluteFullWidth: sty.ModifierSeq = Seq(
    sty.absolutePosition,
    width := "100%"
  )

  lazy val centered: ModifierSeq = Seq(
    styles.left := "50%",
    minWidth := 250,
    svgAttrs.transform := "translate (-50%,-50%)"
  )

  lazy val sidebarStyle: ModifierSeq = Seq(
    backgroundColor  := Colors.LightBlue.cssHash
  )

  lazy val labelerBodyStyle: ModifierSeq = Seq(
    position.absolute,
    top := "80px",
    overflow  := "auto"
  )
  lazy val controlClusterStyle: ModifierSeq = Seq(
    backgroundColor := Colors.FloralWhite.cssHash(),
    position.fixed,
    top := "70px",
    zIndex := 90
  )

  lazy val navbar_FixedTop = "navbar-fixed-top".clazz

  lazy val navbarStyle: ModifierSeq = Seq(
    padding     :=0,
    border      :=0,
    margin      :=0,
    navbar_FixedTop,
    sty.nav,
    navbar_inverse,
    height           := "67px",
    fontSize         := 12,
    color            := Colors.DarkBlue.cssHash,
    backgroundColor  := Colors.LightBlue.cssHash,
    zIndex := 100
  )

  lazy val mainContentStyle: ModifierSeq = Seq(
    jmarginTop       := 30
  )

  lazy val footerStyle: ModifierSeq =
    zeroFringe ++ Seq(
    height          := 1,
    left            := 0,
    jmarginTop      := "5em",
    backgroundColor := Colors.DarkSlateBlue.cssHash,
    width           := "100%"

  )


  // lazy val fileChevronStyle: ModifierSeq = Seq(
  //   lineHeight := "10px",
  //   top := 10,
  //   left := -30,
  //   sty.paddingRight(20)
  // )


  // lazy val mainNav370: ModifierSeq = Seq(
  //   sty.paddingLeft(370),
  //   borderColor := "yellow",
  //   zIndex := 10
  // )

  lazy val canvasContainer: ModifierSeq =
    zeroFringe ++ Seq(
      display.`inline-block`,
      position.relative
    )


  lazy val canvasOverlay: ModifierSeq =
    zeroFringe ++ Seq(
      position.absolute,
      zIndex      :=100
    )

  lazy val fabricCanvasStyle: ModifierSeq =
    zeroFringe ++ Seq(
      position.absolute,
      left    := 0,
      top     := 0
    )

  // lazy val centerpanel: ModifierSeq = Seq(
  //   sty.paddingLeft(15),
  //   sty.paddingRight(15),
  //   relativePosition,
  //   height := "89%",
  //   top := 30,
  //   width := "100%"
  // )

//   lazy val leftpanel: ModifierSeq = Seq(
//     sty.paddingLeft(7),
//     absolutePosition,
//     background  := s"gray none repeat scroll 0 0",
//     height      := "100%",
//     // styles.left := -320,
//     opacity     := 1,
//     overflowY   := "auto",
//     // top         := 37,
//     transition  := "all 0.1 s ease - out 0 s",
//     width       := 320
//   )

}
