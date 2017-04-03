package edu.umass.cs.iesl.watr
package watrcolors
package client


import scaladget.stylesheet.{all => sty}
import scalatags.JsDom.all.{marginTop => jmarginTop, _}

import scalatags.JsDom.{
  svgAttrs,
  styles
}
import utils.Colors
import sty._


object pageStyles {
  lazy val zeroFringe: ModifierSeq = Seq(
    padding     :=0,
    border      :=0,
    margin      :=0
  )

  lazy val absCenterPage: ModifierSeq = Seq(
    position := "fixed",
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

  lazy val navbarStyle: ModifierSeq = Seq(
    navbar_staticTop,
    sty.nav,
    navbar_pills,
    navbar_inverse,
    height           := 30,
    fontSize         := 20,
    color            := Colors.GhostWhite.cssHash,
    backgroundColor  := Colors.DarkSlateBlue.cssHash,
    zIndex := 10
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


  lazy val fileChevronStyle: ModifierSeq = Seq(
    lineHeight := "10px",
    top := 10,
    left := -30,
    sty.paddingRight(20)
  )


  lazy val mainNav370: ModifierSeq = Seq(
    sty.paddingLeft(370),
    borderColor := "yellow",
    zIndex := 10
  )

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

  lazy val fabricCanvas: ModifierSeq =
    zeroFringe ++ Seq(
      position.absolute,
      left    := 0,
      top     := 0
    )

}
