package edu.umass.cs.iesl.watr

trait ScalatagsDefs {
  import scalatags.Text
  import scalatags.text

  object texttags
      extends Text.Cap
      with Text.Util
      with Text.Aggregate
      with scalatags.DataConverters


  type TextTag = Text.TypedTag[String]
  type TextModifier = Text.Modifier

  val TextTag = Text.TypedTag

  object < extends Text.Cap with text.Tags with text.Tags2 with text.SvgTags {
    import texttags._

    def nbsp = raw("&nbsp;")

  }

  object ^ extends Text.Cap with Text.Attrs {
    lazy val x = attr("x")
    lazy val y = attr("y")
    lazy val width = attr("width")
    lazy val height =     attr("height")
    lazy val labelName =  attr("label-name")
    lazy val labelValue = attr("label-value")

  }

  object $ extends Text.Cap with Text.Styles with Text.Styles2

  import texttags._

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
