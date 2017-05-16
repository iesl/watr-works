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

  object svgattr extends JsDom.Cap with JsDom.SvgAttrs

  object ^ extends JsDom.Cap with JsDom.Attrs with JsDom.SvgAttrs {
    // Override conflicting members in jsdom/svg attrs
    override lazy val `class` = svgattr.`class`
    override lazy val id      = svgattr.id
    override lazy val max     = svgattr.max
    override lazy val min     = svgattr.min
    override lazy val style   = svgattr.style
    override lazy val `type`  = svgattr.`type`
    override lazy val xmlns   = svgattr.xmlns

    lazy val labelName        = attr("label-name")
    lazy val labelValue       = attr("label-value")
  }

  object $ extends JsDom.Cap with JsDom.Styles with JsDom.Styles2

  import domtags._

  implicit class RichString(val s: String)  {
    def clazz      = ^.`class` := s
    def id         = ^.`id` := s
    def style      = ^.style := s
    def labelName  = ^.labelName:=s
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



package object client extends ScalatagsDomDefs {
  import org.querki.jquery
  import org.scalajs.dom

  val jQuery = jquery.$


  type ElementTag = JsDom.TypedTag[_ <: dom.Element]


}
