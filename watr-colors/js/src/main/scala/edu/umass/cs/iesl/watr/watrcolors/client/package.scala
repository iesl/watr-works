package edu.umass.cs.iesl.watr
package watrcolors

import scalatags.jsdom
import scalatags.JsDom

trait ScalatagsDomDefs {

  object domtags
      extends JsDom.Cap
      with JsDom.Util
      with JsDom.Aggregate
      with scalatags.DataConverters

  object < extends JsDom.Cap with jsdom.Tags with jsdom.Tags2 with jsdom.SvgTags {
    import domtags._

    def nbsp = raw("&nbsp;")

  }

  object ^ extends JsDom.Cap with JsDom.Attrs {
    lazy val x = attr("x")
    lazy val y = attr("y")
    lazy val width = attr("width")
    lazy val height =     attr("height")
    lazy val labelName =  attr("label-name")
    lazy val labelValue = attr("label-value")
  }

  object $ extends JsDom.Cap with JsDom.Styles with JsDom.Styles2

  import domtags._


  implicit class RichString(val s: String)  {
    def clazz = ^.`class` := s
    def id = ^.`id` := s
    def labelName = ^.labelName:=s
    def labelValue = ^.labelValue:=s
  }

  def fmt = (d: Double) => f"${d}%1.2f"

  implicit class RichDouble(val v: Double)  {
    def attrX      = ^.x      := fmt(v)
    def attrY      = ^.y      := fmt(v)
    def attrWidth  = ^.width  := fmt(v)
    def attrHeight = ^.height := fmt(v)
  }

}


import org.querki.jquery

package object client extends ScalatagsDomDefs {
  val jQuery = jquery.$

}
