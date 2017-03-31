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
  lazy val absCenterPage: ModifierSeq = Seq(
    position := "fixed",
    top := "45%",
    styles.left := "50%",
    minWidth := 250,
    svgAttrs.transform := "translate (-50%,-50%)"
  )

  lazy val centered: ModifierSeq = Seq(
    styles.left := "50%",
    minWidth := 250,
    svgAttrs.transform := "translate (-50%,-50%)"
  )

  lazy val navbarStyle: ModifierSeq = Seq(
    height           := 30,
    color            := Colors.GhostWhite.cssHash,
    backgroundColor  := Colors.DarkSlateBlue.cssHash

  )
  lazy val footerStyle: ModifierSeq = Seq(
    padding         := 0,
    margin          := 0,
    border          := 0,
    height          := 1,
    left            := 0,
    jmarginTop      := "5em",
    backgroundColor := Colors.DarkSlateBlue.cssHash,
    width           := "100%"

  )

  lazy val absoluteFullWidth: sty.ModifierSeq = Seq(
    sty.absolutePosition,
    width := "100%"
  )

}
